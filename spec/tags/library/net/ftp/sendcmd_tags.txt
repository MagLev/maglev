fails:Net::FTP#sendcmd raises no error when the response code is 1xx, 2xx or 3xx
fails:Net::FTP#sendcmd raises a Net::FTPPermError when the response code is 5xx
fails:Net::FTP#sendcmd raises a Net::FTPProtoError when the response code is not between 1xx-5xx
