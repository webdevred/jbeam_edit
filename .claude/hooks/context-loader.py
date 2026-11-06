#!/usr/bin/env python3
"""UserPromptSubmit hook: adds a context file to the turn when the prompt names its topic."""

from __future__ import annotations

import json
import re
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

TOPICS = {
    "profiling": (
        re.compile(r"\b(profil|alloc|cost cent|flame ?graph)|\+RTS", re.IGNORECASE),
        "PROFILING_DOCS.md",
    ),
}


def already_loaded(session_id: str, topic: str) -> bool:
    marker = Path(tempfile.gettempdir()) / f"jbeam-edit-context-{session_id}-{topic}"
    if marker.exists():
        return True
    marker.touch()
    return False


def main() -> int:
    event = json.loads(sys.stdin.buffer.read().decode("utf-8"))
    prompt = event.get("prompt")
    session_id = event.get("session_id")
    if not isinstance(prompt, str) or not isinstance(session_id, str):
        return 0
    sections = [
        (REPO_ROOT / doc).read_text(encoding="utf-8")
        for topic, (pattern, doc) in TOPICS.items()
        if pattern.search(prompt) and not already_loaded(session_id, topic)
    ]
    if sections:
        print(json.dumps({
            "hookSpecificOutput": {
                "hookEventName": "UserPromptSubmit",
                "additionalContext": "\n".join(sections),
            }
        }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
