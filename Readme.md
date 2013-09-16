Learning as i go, Quartz PdfKit <-> Haskell wrapper.

Check [http://ashleytowns.id.au/pdfkiths/frames.html](http://ashleytowns.id.au/pdfkiths/frames.html) for more info

## Obj-c calls
calls into obj-c are done via `$<-` and `$<<-` for example:

    -- [[NSAutoreleasePool alloc] init]
    ("NSAutoreleasePool" $<- "alloc") >>= (\pool -> pool $<- "init")

    -- $<<- takes parameters as a list
    allocUrl <- "NSUrl" $<- "alloc"
    url <- (allocUrl $<<- "initWithFilePath:") [APath]

## PdfKit

Todo: ?!
