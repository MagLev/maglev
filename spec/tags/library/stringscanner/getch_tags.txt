fails:StringScanner#getch is multi-byte character sensitive
fails:StringScanner#getch returns an instance of String when passed a String subclass
fails:StringScanner#getch taints the returned String if the input was tainted
