Learning as i go, Quartz PdfKit <-> Haskell wrapper

# Obj-c calls
calls into obj-c are done via `$<-` and `$<<-` for example:

    -- [[NSAutoreleasePool alloc] init]
    ("NSAutoreleasePool" $<- "alloc") >>= (\pool -> pool $<- "init")

    -- $<<- takes parameters as a list
    allocUrl <- "NSUrl" $<- "alloc"
    url <- (allocUrl $<<- "initWithFilePath:") [APath]

check testannotations.hs for more. 

