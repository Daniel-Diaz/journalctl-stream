## 0.4.0.0
* Changed arguments of `entryStream`. It now takes a value of type
  `StreamStart`. This value can be used to indicate where the stream
  will start. The unit filter argument is gone. Instead, use conduit
  functions to filter the stream.

## 0.3.0.0
* Fix for fields: `entryPID`, `entryProcess`, `entryExecutable`.
  They are optional now, as they should have been.

## 0.2.0.0
* Added new fields: `entryNamespace`, `entryProcess`, `entryExecutable`.

## 0.1.0.0
* First release.
