# scribblr (development version)

### Fixes

- Pressing Tab in text area is now undoable (with Ctrl+Z) (#19)
- Added instructions to install from my R-universe in README (#13)
- Import `utils`.

### Testing
- Added `testthat` infrastructure.

# scribblr 0.2.0

### New features

- Support for exporting notes as GitHub Issues

### UI changes

- New tabstrip interface
- Added "About" tab

# scribblr 0.1.0

### New features

- Add support for multiple notes

### Backward incompatible changes

- `scribblr` notes and settings are now stored in the ".scribblr" directory 
under the project's root (previously this was a single ".scribblr" text file).

# scribblr 0.0.4

### New features

- New logo for `{scribblr}`

### UI change

- "Close" button becomes "Done" button
- UI displays the logo

# scribblr 0.0.3

### New features

- Notes can now be exported to an external file (#4)
- Markdown previews now available (#3)
- Added placeholder text explaining when and where `{scribblr}` notes are stored (#4)

### Fixes

- Pressing Tab in text input area is now used to add indentation (#2)

# scribblr 0.0.2

* `scribblr` file paths are now generated with `file.path()`, for platform independence.
* Added GitHub link to user interface.

# scribblr 0.0.1

* `scribblr` now imports `usethis`.
