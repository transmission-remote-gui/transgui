"""Regression tests for Pascal routine extraction, without requiring FPC."""

import unittest

from test_copy_magnet import extract_routine


class ExtractionTests(unittest.TestCase):
    """Keep adjacent methods and commented declarations out of the compiled fragment."""

    def test_method_boundaries(self):
        """Every supported top-level method form terminates the selected routine."""
        target = "procedure TMainForm.MenuItem101Click(Sender: TObject);\nbegin\nend;\n\n"
        following = (
            "procedure TMainForm.Next;", "function TMainForm.Next: Boolean;",
            "constructor TMainForm.Create;", "destructor TMainForm.Destroy;",
            "class procedure TMainForm.Next;", "class function TMainForm.Next: Boolean;",
            "class operator TRecord.Equal(a, b: TRecord): Boolean;",
            "operator +(a, b: TRecord): TRecord;", "procedure OtherRoutine;",
            "function TOtherForm.Next: Boolean;", "INITIALIZATION", "finalization", "end.",
        )
        for declaration in following:
            with self.subTest(declaration=declaration):
                source = target + declaration + "\nbegin\nend;\n"
                self.assertEqual(extract_routine(source, "TMainForm.MenuItem101Click"), target)

    def test_nested_routine_and_blocks(self):
        """Indented local routines and nested blocks stay inside the implementation."""
        target = (
            "procedure TMainForm.MenuItem101Click(Sender: TObject);\n"
            "  function Local: Boolean;\n  begin\n    Result:=True;\n  end;\n"
            "begin\n  try\n    if Local then begin\n    end;\n  finally\n  end;\nend;\n"
        )
        self.assertEqual(extract_routine(target + "end.\n", "TMainForm.MenuItem101Click"), target)

    def test_comments_and_strings(self):
        """Fake declarations inside comments and string literals are ignored."""
        target = (
            "procedure TMainForm.MenuItem101Click(Sender: TObject);\nbegin\n"
            "{\nfunction TMainForm.Fake: Boolean;\n}\n"
            "(*\nconstructor TMainForm.Fake;\n*)\n"
            "// procedure TMainForm.Fake;\n"
            "  WriteLn('procedure TMainForm.Fake; it''s only text');\nend;\n"
        )
        self.assertEqual(extract_routine(target + "end.\n", "TMainForm.MenuItem101Click"), target)

    def test_case_insensitive_declarations(self):
        """Pascal declaration casing does not affect routine selection."""
        target = "PROCEDURE tmainform.menuitem101click(Sender: TObject);\nBEGIN\nEND;\n"
        self.assertEqual(extract_routine(target + "END.\n", "TMainForm.MenuItem101Click"), target)

    def test_invalid_boundaries(self):
        """Missing, duplicate, unfinished and unsupported fragments fail explicitly."""
        target = "procedure TMainForm.MenuItem101Click(Sender: TObject);\nbegin\nend;\n"
        sources = (
            "procedure Other;\nbegin\nend;\nend.\n",
            target + target + "end.\n", target,
            "procedure TMainForm.MenuItem101Click(Sender: TObject);\nbegin\nend.\n",
            target + "var Unexpected: Integer;\nprocedure Next;\nbegin\nend;\n",
        )
        for source in sources:
            with self.subTest(source=source), self.assertRaises(ValueError):
                extract_routine(source, "TMainForm.MenuItem101Click")


if __name__ == "__main__":
    unittest.main()
