#!/usr/bin/env python3
"""Generate the README Excalidraw scenes.

The scenes are data. This generator keeps geometry and palette in one
place so a README figure can be regenerated after an architecture change.
"""

from __future__ import annotations

import json
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parent
WHITE = "#ffffff"
INK = "#1e1e1e"
MUTED = "#5c5f66"

PALETTE = {
    "compiler": ("#1971c2", "#d0ebff"),
    "runtime": ("#9c36b5", "#f3d9fa"),
    "analysis": ("#f08c00", "#fff3bf"),
    "risk": ("#e03131", "#ffe3e3"),
    "neutral": ("#495057", "#f1f3f5"),
}

FONT_TITLE = 32
FONT_BODY = 22
FONT_NOTE = 18
FONT_ARROW = 16
STROKE = 2
ROUGHNESS = 2
FILL_STYLE = "hachure"
LINE_HEIGHT = 1.25


def seed_for(element_id: str) -> int:
    """Return a stable Excalidraw seed for an element id."""
    return zlib.crc32(element_id.encode("utf-8")) % 1_000_000


def base_element(element_id: str, kind: str, x: int, y: int, width: int, height: int, role: str) -> dict:
    """Build the common Excalidraw fields for a shape or label."""
    stroke, fill = PALETTE[role]
    seed = seed_for(element_id)
    return {
        "id": element_id,
        "type": kind,
        "x": x,
        "y": y,
        "width": width,
        "height": height,
        "angle": 0,
        "strokeColor": stroke,
        "backgroundColor": fill,
        "fillStyle": FILL_STYLE,
        "strokeWidth": STROKE,
        "strokeStyle": "solid",
        "roughness": ROUGHNESS,
        "opacity": 100,
        "groupIds": [],
        "frameId": None,
        "roundness": {"type": 3} if kind == "rectangle" else None,
        "seed": seed,
        "version": 1,
        "versionNonce": seed,
        "isDeleted": False,
        "boundElements": [],
        "updated": 0,
        "link": None,
        "locked": False,
    }


class Scene:
    """One Excalidraw scene with box, label, and arrow helpers."""

    def __init__(self, name: str, width: int, height: int) -> None:
        self.name = name
        self.width = width
        self.height = height
        self.elements: list[dict] = []
        self._counter = 0

    def _id(self, prefix: str) -> str:
        self._counter += 1
        return f"{self.name}_{prefix}_{self._counter:02d}"

    def box(self, x: int, y: int, width: int, height: int, title: str, body: str, role: str) -> str:
        """Add a hand-drawn box with centered bound text."""
        box_id = self._id("box")
        text_id = self._id("text")
        rectangle = base_element(box_id, "rectangle", x, y, width, height, role)
        rectangle["boundElements"] = [{"type": "text", "id": text_id}]
        self.elements.append(rectangle)

        lines = [title] + ([body] if body else [])
        text = base_element(text_id, "text", x + 12, y + 10, width - 24, height - 20, "neutral")
        text.update(
            {
                "strokeColor": INK,
                "backgroundColor": "transparent",
                "roundness": None,
                "boundElements": [],
                "text": "\n".join(lines),
                "originalText": "\n".join(lines),
                "fontSize": FONT_BODY,
                "fontFamily": 1,
                "textAlign": "center",
                "verticalAlign": "middle",
                "containerId": box_id,
                "lineHeight": LINE_HEIGHT,
            }
        )
        self.elements.append(text)
        return box_id

    def label(self, x: int, y: int, width: int, text: str, font_size: int = FONT_NOTE, role: str = "neutral") -> None:
        """Add standalone text, used for titles, notes, and arrow labels."""
        label_id = self._id("label")
        element = base_element(label_id, "text", x, y, width, font_size * 2, role)
        element.update(
            {
                "strokeColor": MUTED if role == "neutral" else INK,
                "backgroundColor": "transparent",
                "roundness": None,
                "boundElements": [],
                "text": text,
                "originalText": text,
                "fontSize": font_size,
                "fontFamily": 1,
                "textAlign": "left",
                "verticalAlign": "top",
                "containerId": None,
                "lineHeight": LINE_HEIGHT,
            }
        )
        self.elements.append(element)

    def arrow(self, x1: int, y1: int, x2: int, y2: int, color_role: str = "neutral") -> None:
        """Add a straight arrow with no binding, so the PNG is deterministic."""
        arrow_id = self._id("arrow")
        element = base_element(arrow_id, "arrow", x1, y1, abs(x2 - x1), abs(y2 - y1), color_role)
        element.update(
            {
                "strokeColor": MUTED,
                "backgroundColor": "transparent",
                "fillStyle": FILL_STYLE,
                "roundness": {"type": 2},
                "points": [[0, 0], [x2 - x1, y2 - y1]],
                "lastCommittedPoint": None,
                "startBinding": None,
                "endBinding": None,
                "startArrowhead": None,
                "endArrowhead": "arrow",
            }
        )
        self.elements.append(element)

    def write(self) -> None:
        """Write the scene with a white export background."""
        scene = {
            "type": "excalidraw",
            "version": 2,
            "source": "https://excalidraw.com",
            "elements": self.elements,
            "appState": {
                "viewBackgroundColor": WHITE,
                "exportBackground": True,
                "gridSize": None,
            },
            "files": {},
        }
        target = ROOT / f"{self.name}.excalidraw"
        target.write_text(json.dumps(scene, indent=2) + "\n")


def arrow_label(scene: Scene, x: int, y: int, text: str, width: int = 150) -> None:
    """Place a short arrow label near the midpoint."""
    scene.label(x, y, width, text, FONT_ARROW)


def architecture() -> None:
    """Draw the pass pipeline as a three-column serpentine."""
    scene = Scene("architecture", 1520, 940)
    scene.label(40, 40, 1400, "Compiler pipeline: source text to a lazy value", FONT_TITLE)
    scene.box(40, 150, 420, 100, "Source text", "UTF-8 input", "neutral")
    scene.box(550, 150, 420, 100, "Lexer + layout", "tokens and braces", "compiler")
    scene.box(1060, 150, 420, 100, "Parser", "surface AST", "compiler")

    scene.arrow(460, 200, 550, 200)
    arrow_label(scene, 470, 265, "characters", 100)
    scene.arrow(970, 200, 1060, 200)
    arrow_label(scene, 980, 265, "tokens", 80)

    scene.box(1060, 340, 420, 100, "Renamer", "unique binders", "compiler")
    scene.box(550, 340, 420, 100, "Desugar", "do and comprehensions", "compiler")
    scene.box(40, 340, 420, 100, "Kind checker", "synonyms and kinds", "analysis")

    scene.arrow(1270, 250, 1270, 340)
    arrow_label(scene, 1280, 285, "AST", 60)
    scene.arrow(1060, 390, 970, 390)
    arrow_label(scene, 980, 455, "declarations", 130)
    scene.arrow(550, 390, 460, 390)
    arrow_label(scene, 470, 455, "module", 80)

    scene.box(40, 530, 420, 100, "Infer", "types and evidence", "analysis")
    scene.box(550, 530, 420, 100, "Elaborate", "dictionaries and cases", "compiler")
    scene.box(1060, 530, 420, 100, "Core", "cases and dictionaries", "runtime")

    scene.arrow(250, 440, 250, 530)
    arrow_label(scene, 260, 475, "types", 70)
    scene.arrow(460, 580, 550, 580)
    arrow_label(scene, 470, 645, "evidence", 100)
    scene.arrow(970, 580, 1060, 580)
    arrow_label(scene, 980, 645, "dictionaries", 130)

    scene.box(1060, 720, 420, 100, "Eval", "call-by-need thunks", "runtime")
    scene.box(550, 720, 420, 100, "REPL / CLI", "values and IO", "runtime")

    scene.arrow(1270, 630, 1270, 720)
    arrow_label(scene, 1280, 665, "cases", 70)
    scene.arrow(1060, 770, 970, 770)
    arrow_label(scene, 980, 835, "values", 80)
    scene.label(40, 860, 1400, "Arrows carry the representation handed to the next pass.")
    scene.write()


def inference() -> None:
    """Draw the constraint, scheme, and dictionary flow in three columns."""
    scene = Scene("inference", 1520, 820)
    scene.label(40, 40, 1400, "Inference: constraints in, dictionaries out", FONT_TITLE)
    scene.box(40, 140, 420, 100, "Judgment", "Γ ⊢ e : τ", "analysis")
    scene.box(550, 140, 420, 100, "Constraints", "τ₁ ~ τ₂, C a", "analysis")
    scene.box(1060, 140, 420, 100, "Unifier", "S = mgu(τ₁, τ₂)", "analysis")

    scene.arrow(460, 190, 550, 190)
    arrow_label(scene, 470, 255, "generate", 100)
    scene.arrow(970, 190, 1060, 190)
    arrow_label(scene, 980, 255, "solve", 70)

    scene.box(1060, 330, 420, 100, "Scheme", "∀a. Q ⇒ τ", "analysis")
    scene.box(550, 330, 420, 100, "Method use", "m : ∀a. C a ⇒ τ", "compiler")
    scene.box(40, 330, 420, 100, "Predicate", "C T", "compiler")

    scene.arrow(1270, 240, 1270, 330)
    arrow_label(scene, 1280, 275, "generalize", 100)
    scene.arrow(1060, 380, 970, 380)
    arrow_label(scene, 980, 445, "instantiate", 110)
    scene.arrow(550, 380, 460, 380)
    arrow_label(scene, 470, 445, "resolve", 90)

    scene.box(40, 520, 420, 100, "Instance search", "C T → instance", "risk")
    scene.box(550, 520, 420, 100, "Dictionary", "d :: C T", "runtime")
    scene.box(1060, 520, 420, 100, "Call", "m d v", "compiler")

    scene.arrow(250, 430, 250, 520)
    arrow_label(scene, 260, 465, "select", 70)
    scene.arrow(460, 570, 550, 570)
    arrow_label(scene, 470, 635, "build", 70)
    scene.arrow(970, 570, 1060, 570)
    arrow_label(scene, 980, 635, "apply", 70)

    scene.label(40, 700, 1400, "Occurs check rejects τ = τ → σ. Signatures skolemize ∀-variables.")
    scene.label(40, 740, 1400, "No machine-checked soundness proof. The suite checks these invariants.")
    scene.write()


def verification() -> None:
    """Draw the evidence split across syntax, static semantics, and runtime."""
    scene = Scene("verification", 1520, 760)
    scene.label(40, 40, 1400, "Verification: 146 cases cover each pass", FONT_TITLE)
    scene.box(60, 130, 440, 170, "Syntax", "38 cases\nspans, layout, scope", "compiler")
    scene.box(540, 130, 440, 170, "Static semantics", "37 cases\nkinds, schemes, dictionaries", "analysis")
    scene.box(1020, 130, 440, 170, "Runtime", "71 cases\nvalues, laziness, IO", "runtime")
    scene.box(260, 380, 1000, 110, "Negative evidence", "23 cases assert rejection or runtime failure", "risk")
    scene.label(40, 550, 1400, "Packaging: cabal sdist and tarball test.")
    scene.label(40, 590, 1400, "The suite checks implemented invariants. It is not a proof of type soundness.")
    scene.label(40, 630, 1400, "GHC 9.10.3, Cabal 3.16, 146 passing cases.")
    scene.write()


if __name__ == "__main__":
    architecture()
    inference()
    verification()
