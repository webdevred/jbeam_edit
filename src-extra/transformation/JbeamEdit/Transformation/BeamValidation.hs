module JbeamEdit.Transformation.BeamValidation (
  validateBeams,
  extractVertexNames,
  extractFileBeams,
  findInvalidRefs,
  findDuplicateBeams,
) where

import Control.Monad (forM_, unless)
import Data.Foldable (foldl')
import Data.List.NonEmpty (toList)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import JbeamEdit.Core.Node (Node)
import JbeamEdit.IOUtils (humanJoin, putErrorStringLn, tryReadFile)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.BeamExtraction
import JbeamEdit.Transformation.Config (defaultBreakpoints)
import JbeamEdit.Transformation.Types
import JbeamEdit.Transformation.VertexExtraction
import System.Directory.OsPath (getCurrentDirectory, listDirectory)
import System.OsPath (OsPath)

parseFileIO :: OsPath -> IO (OsPath, Node, Set Text, [Beam])
parseFileIO fp = do
  eBs <- tryReadFile [] fp
  bs <- case eBs of
    Left err -> fail $ "Failed to read file " ++ show fp ++ ": " ++ show err
    Right content -> pure content

  node <- case parseNodes bs of
    Left err -> fail $ "Failed to parse nodes in file " ++ show fp ++ ": " ++ T.unpack err
    Right n -> pure n

  verts <- case extractVertexNames node of
    Left err ->
      fail $ "Failed to get vertices from file " ++ show fp ++ ": " ++ T.unpack err
    Right vs -> pure vs

  pure (fp, node, verts, extractFileBeams node)

allVerticesInForest :: VertexForest -> Set Text
allVerticesInForest =
  foldMap
    (foldMap (S.fromList . map anVertexName . toList . tAnnotatedVertices))
    . M.elems

extractVertexNames :: Node -> Either Text (Set Text)
extractVertexNames node =
  case getVertexForest defaultBreakpoints verticesQuery node of
    Left err -> Left err
    Right (_, _, vf) -> Right (allVerticesInForest vf)

extractFileBeams :: Node -> [Beam]
extractFileBeams node =
  case extractBeams node of
    Right bs
      | V.length bs >= 1 -> snd (extractBeamsWithMeta bs)
    _ -> []

findInvalidRefs :: Set Text -> [Beam] -> [(Text, Text, Set Text)]
findInvalidRefs allVertexNames beams =
  [ (n1, n2, invalid)
  | Beam (BeamPair n1 n2) _ <- beams
  , let invalid = S.difference (S.fromList [n1, n2]) allVertexNames
  , not (S.null invalid)
  ]

findDuplicateBeams :: [(a, [Beam])] -> [(Beam, Int)]
findDuplicateBeams files =
  let occurrences =
        foldl'
          (\acc (_, beams) -> foldl' (\m b -> M.insertWith (+) b (1 :: Int) m) acc beams)
          M.empty
          files
   in [(b, n) | (b, n) <- M.toList occurrences, n > 1]

validateBeamRefs :: OsPath -> Set Text -> [Beam] -> IO ()
validateBeamRefs fp allVertexNames beams =
  forM_ beams $ \(Beam (BeamPair n1 n2) _) -> do
    let invalid = S.difference (S.fromList [n1, n2]) allVertexNames
    unless (S.null invalid) . putErrorStringLn $
      "Beam in file "
        ++ show fp
        ++ " references unknown vertices: "
        ++ T.unpack (humanJoin "and" (S.toList invalid))
        ++ " in [\""
        ++ T.unpack n1
        ++ "\", \""
        ++ T.unpack n2
        ++ "\"]"

type BeamOccurrences = M.Map Beam [OsPath]

addBeamsFromFile :: OsPath -> [Beam] -> BeamOccurrences -> BeamOccurrences
addBeamsFromFile fp beams acc =
  foldl' (\m b -> M.insertWith (++) b [fp] m) acc beams

reportDuplicates :: BeamOccurrences -> IO ()
reportDuplicates occurrences =
  forM_ (M.toList occurrences) $ \(Beam (BeamPair n1 n2) _, locations) ->
    unless (length locations <= 1) . putErrorStringLn $
      "Duplicate beam [\""
        ++ T.unpack n1
        ++ "\", \""
        ++ T.unpack n2
        ++ "\"] with identical metadata found "
        ++ show (length locations)
        ++ " times in: "
        ++ T.unpack (humanJoin "and" (map (T.pack . show) locations))

validateBeams :: Maybe OsPath -> IO ()
validateBeams inputFile = do
  cwd <- getCurrentDirectory
  allFiles <- filterJbeamFiles [] <$> listDirectory cwd

  parsedFiles <- mapM parseFileIO allFiles

  let allVertexNames = foldMap (\(_, _, verts, _) -> verts) parsedFiles
      filesToCheck = case inputFile of
        Just f -> [(fp, beams) | (fp, _, _, beams) <- parsedFiles, fp == f]
        Nothing -> [(fp, beams) | (fp, _, _, beams) <- parsedFiles]

  mapM_ (\(fp, beams) -> validateBeamRefs fp allVertexNames beams) filesToCheck

  let allBeamOccurrences =
        foldl' (\acc (fp, beams) -> addBeamsFromFile fp beams acc) M.empty filesToCheck
  reportDuplicates allBeamOccurrences
