"""Apply only the reviewed copy-magnet marker rollback to the preparation checkout."""
from pathlib import Path

path = Path('main.pas')
source = path.read_bytes()
start = source.index(b'procedure TMainForm.MenuItem101Click(Sender: TObject);')
end = source.index(b'\nprocedure TMainForm.', start + 1)
handler = source[start:end]
old = b'''    FLastClipboardLink := Magnets.Text;   // To Avoid TransGUI detect again this existing links
    Clipboard.AsText := Magnets.Text;
'''
new = b'''    ClipboardText:=Magnets.Text;
    PreviousClipboardLink:=FLastClipboardLink;
    // Suppress detection of our own links during the clipboard write.
    FLastClipboardLink:=ClipboardText;
    try
      Clipboard.AsText:=ClipboardText;
    except
      FLastClipboardLink:=PreviousClipboardLink;
      raise;
    end;
'''
declaration = b'  MagnetLink: TJSONString;\n'
assert handler.count(old) == 1, 'clipboard assignment block changed'
assert handler.count(declaration) == 1, 'handler declaration changed'
handler = handler.replace(declaration, declaration + b'  ClipboardText, PreviousClipboardLink: string;\n')
handler = handler.replace(old, new)
path.write_bytes(source[:start] + handler + source[end:])
