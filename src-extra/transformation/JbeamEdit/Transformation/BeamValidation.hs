module JbeamEdit.Transformation.BeamValidation (validateBeams) where

import Control.Monad (unless)
import Data.List.NonEmpty (toList)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (thd3)
import Data.Vector qualified as V
import JbeamEdit.Core.Node (Node)
import JbeamEdit.IOUtils (humanJoin, tryReadFile)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.BeamExtraction
import JbeamEdit.Transformation.Config (defaultBreakpoints)
import JbeamEdit.Transformation.Types
import JbeamEdit.Transformation.VertexExtraction
import System.Directory.OsPath (getCurrentDirectory, listDirectory)
import System.OsPath (OsPath)

parseFileIO :: OsPath -> IO (OsPath, Node, Set Text)
parseFileIO fp = do
  eBs <- tryReadFile [] fp
  bs <- case eBs of
    Left err -> fail $ "Failed to read file " ++ show fp ++ ": " ++ show err
    Right content -> pure content

  node <- case parseNodes bs of
    Left err -> fail $ "Failed to parse nodes in file " ++ show fp ++ ": " ++ T.unpack err
    Right n -> pure n

  verts <- case getVertexForest defaultBreakpoints verticesQuery node of
    Left err ->
      fail $ "Failed to get vertices from file " ++ show fp ++ ": " ++ T.unpack err
    Right (_, _, vf) -> pure (allVerticesInForest vf)

  pure (fp, node, verts)

allVerticesInForest :: VertexForest -> Set Text
allVerticesInForest =
  foldMap
    (foldMap (S.fromList . map anVertexName . toList . tAnnotatedVertices))
    . M.elems

validateBeamsInDirectory :: Set Text -> (OsPath, Node) -> IO ()
validateBeamsInDirectory allVertexNames (fp, node) =
  do
    beamNodes <- case extractBeams node of
      Right bs
        | V.length bs >= 1 -> pure (V.tail bs)
        | otherwise ->
            fail $ "Failed to extract beams from file " ++ show fp ++ ": empty beam array"
      Left err ->
        fail $ "Failed to extract beams from file " ++ show fp ++ ": " ++ T.unpack err
    mapM_ (validateBeam fp allVertexNames) beamNodes

validateBeam :: OsPath -> Set Text -> Node -> IO ()
validateBeam fp allVertexNames beamNode =
  case possiblyBeam beamNode of
    Left _ -> fail $ "Invalid beam node in file " ++ show fp
    Right Nothing -> pure ()
    Right (Just vertexNames) ->
      let vertexNames' = V.toList vertexNames
          invalid = S.difference (S.fromList vertexNames') allVertexNames
       in unless (S.null invalid) . fail $
            "Beam in file "
              ++ show fp
              ++ " references unknown vertices: "
              ++ T.unpack (humanJoin "and" (S.toList invalid))
              ++ " in "
              ++ show vertexNames'

validateBeams :: Maybe OsPath -> IO ()
validateBeams inputFile = do
  cwd <- getCurrentDirectory
  allFiles <- filterJbeamFiles [] <$> listDirectory cwd

  parsedFiles <- mapM parseFileIO allFiles

  let allVertexNames = foldMap thd3 parsedFiles
      filesToCheck = case inputFile of
        Just f -> [(fp, node) | (fp, node, _) <- parsedFiles, fp == f]
        Nothing -> [(fp, node) | (fp, node, _) <- parsedFiles]
   in mapM_ (validateBeamsInDirectory allVertexNames) filesToCheck
