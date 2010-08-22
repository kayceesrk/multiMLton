val urlString = hd (CommandLine.arguments ())
val url = valOf (Url.fromString (urlString))
val istrm = Http.fetch {head = false, headers = [], post = NONE, proxy = NONE, url = url}
val outputFile = Out.openOut ("downloadedFile")
val _ = In.outputAll (istrm, outputFile)
