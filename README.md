# CS3031 - Project 1: A Web Proxy Server

## Patrick Geoghegan - 13320590

### HTTP and HTTPS Requests

The proxy connects to the browser using TCP. The proxy waits and listens for a request from the browser. Once an URL has been entered into the browser, the request will be sent to the proxy. First, the request is checked to see if it points to a blocked website, then, if unblocked, the proxy handles the request based on the request type (e.g GET, HEAD, CONNECT, and so on). Next, a header request is sent to the URL to check if the page can be loaded from cached data. If not, the request is sent using Haskell's Network.HTTP.Conduit library. Once a response is obtained, it is sent back to the browser.

In the case of CONNECT requests, which are used for HTTPS requests, a connection is made to the request URL using TLS. Using the same HTTP library as above, a request is sent and a response is received. The ```manager``` created in ```handleConnect``` is used to handle security measures throughout the request.

```Haskell
-------------------
-- HTTP REQUESTS --
-------------------

{-
  handleRequest(request_type, request_url):
    Creates a correctly formatted request.
    Returns: http_request
-}
handleRequest :: String -> String -> IO Co.Request
handleRequest rType rURL = do
  let sUrl = splitOn ":" rURL
  (sepUrl, rPort) <- if ((head sUrl /= "http") && (head sUrl /= "https"))
    then return (head sUrl, (read (last sUrl)) :: Int)
    else return (rURL, -1)
  request <- Co.parseRequest (rType ++ " " ++ sepUrl)
  if (rPort == -1)
    then return request
    else return request{Co.port = rPort}

--------------------
-- HTTPS REQUESTS --
--------------------

{-
  handleConnect(request_url):
    Handles CONNECT http requests. Uses TLS to connect to secured
    webpages.
    Returns: request_response
-}
handleConnect :: String -> IO (Maybe B.ByteString)
handleConnect connectUrl = do
  let splitUrl = splitOn ":" connectUrl
  let connectHost = head splitUrl
  let connectPort = last splitUrl
  req <- Co.parseRequest $ "https://" ++ connectHost
  print req
  let settings = Co.mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- Co.newManager settings
  res <- Co.httpLbs req manager
  print res
  return (Just (pack $ parseHTML (Co.responseBody res)))
```

### Dynamically Block URLs

A list of blocked URLs is maintain in the blockSites.txt. I took this approach due to Haskell's nature of not providing use of global variables. They are kept in the text file in the form of a list (array) of Strings. To check if a given URL is blocked, the contetns of the file is read and compared to the given URL. If it exists within the list, then a 403 Forbiddent Response is returned to the browser.

URLs can be added and removed from the list through the management consolse. By typing the command ```block <url>```, the ```<url>``` will be added to the list of blocked sites, and the browser can no longer access that web site. Similarly, ```unblock <url>``` will remove a url from the list.

```Haskell
-------------------
-- BLOCKING URLS --
-------------------
{-
  blockSite(url):
    Adds a URL to the list of blocked URLs.
-}
blockSite :: String -> IO ()
blockSite newSite = do
  contents <- readFile "blockSites.txt"
  let siteList = read contents :: [String]
  tmp_handle <- openFile "blockSites.temp" ReadWriteMode
  hPutStr tmp_handle (show (siteList ++ [newSite]))
  hClose tmp_handle
  copyFile "blockSites.temp" "blockSites.txt"
  removeFile "blockSites.temp"

{-
  unblockSite(url):
    Removes a URL from the list of blocked URLs.
-}
unblockSite :: String -> IO ()
unblockSite site = do
  contents <- readFile "blockSites.txt"
  let siteList = read contents :: [String]
  when (site `elem` siteList) $ do
    tmp_handle <- openFile "blockSites.temp" ReadWriteMode
    hPutStr tmp_handle (show (siteList \\ [site]))
    hClose tmp_handle
    copyFile "blockSites.temp" "blockSites.txt"
    removeFile "blockSites.temp"

{-
  isBlocked(url):
    Checks if a URL is in the list of blocked URLs.
    Returns: blocked
-}
isBlocked :: String -> IO Bool
isBlocked site = do
  blockedSites <- getBlockedSites
  return (site `elem` blockedSites)
```

### Caching Requests

Before a full request is sent, a HEAD request is sent to the requested URL. This will return the header of the web page. From this data we can determine when the page was last edited or modified. From this we can determine if we load the cached data and save bandwidth.

* If a request is made and no cached data exists for that URL, then we carry out the request and load that data into the cache.

* Similarly, if a request is made and the cached data is out of date, we carry out the request and update the cached data.

* If the cached data is current, then we don't carry out the request. Instead we simply load from the cache.

The cached data is stored in text files, where the names of the files are based on the request URL. This makes it straightforward to write to and read from cached data.

```Haskell
-------------
-- CACHING --
-------------

{-
  handleCache(request_type, request_url, head_resposne)
    Compares the headers with cached data to determine if a
    webpage can be loaded from cached data.
    Return: request_data_either_cached_or_fresh
-}
handleCache :: String -> String -> Co.Response ByteString -> IO (Maybe B.ByteString)
handleCache reqType url resp = do
  -- These files cannot be cached
  if (".css" `isInfixOf` url || ".js" `isInfixOf` url || ".svg" `isInfixOf` url)
    then return Nothing
    else do
      -- Check if the HEAD response conatins a last modified header
      if (last_modified_header_exists)
        then do
          -- Extract the Last-Modifed field from the HEAD response
          request_mod_time <- resp
          -- Check to see if the cached data for the request URL exists
          if (cached_data_exists)
            then do
              -- Extract the Last-Modified field from the cached data
              cach_mod_time <- getFrom cached_data
              -- Compare request Last-Modified time and cached Last-Modified time
              if (cache_is_out_of_date)
                then do
                  -- The cached data is out of date.
                  -- Carry out request and update cache.
                  response <- do_request reqType url
                  cacheData response
                  return response
                else do
                  -- Cache is in date. Return cached data
                  return cached_data
            else do
              -- Cached data does not exist.
              -- Carry out request and cache response
              response <- do_request reqType url
              cacheData response
              return response
        -- The request HEAD headers does not contain a Last-Modifed header.
        -- Return nothing
        else return Nothing
```

### Handle multiple requests

Using Haskell's Control.Concurrent library, every time the browser sends a request, the handling of that request is forked onto a new thread. This also means that multiple browsers can connect to the proxy and their requests will be handled conncurently on separate threads.

```Haskell
----------------
-- MAIN LOGIC --
----------------

{-
  tcpListener:
    Starts the proxy and listens for the connection from the browser.
    Once it hears a request from the browser, it passes the request
    into the main logic of the proxy. Uses TCP.
-}
tcpListener :: IO ()
tcpListener = Tcp.listen (Tcp.Host "127.0.0.1") "3100" $ \(listenSocket, listenAddr) -> do
  forever . Tcp.acceptFork listenSocket $ \(connSocket, remoteAddr) -> do
    msg <- Tcp.recv connSocket 8192
    case msg of
      Just m -> do
        resp <- runSocket m
        case resp of
          Nothing -> putStr ""
          Just r -> Tcp.send connSocket r
      -- No response, do nothing
      Nothing -> print ""
```

In the above code, ```Tcp.acceptFork``` will create a new thread every time it hears a new request from a browser.

## Full Code

```Haskell
type DateTime = (Int, Int, Int, Int, Int, Int)

main :: IO ()
main = do
  mainId <- forkIO tcpListener
  managementConsole (Just mainId)

----------------
-- MAIN LOGIC --
----------------

{-
  tcpListener:
    Starts the proxy and listens for the connection from the browser.
    Once it hears a request from the browser, it passes the request
    into the main logic of the proxy. Uses TCP.
-}
tcpListener :: IO ()
tcpListener = Tcp.listen (Tcp.Host "127.0.0.1") "3100" $ \(listenSocket, listenAddr) -> do
  forever . Tcp.acceptFork listenSocket $ \(connSocket, remoteAddr) -> do
    msg <- Tcp.recv connSocket 8192
    case msg of
      Just m -> do
        resp <- runSocket m
        case resp of
          Nothing -> putStr ""
          Just r -> Tcp.send connSocket r
      -- No response, do nothing
      Nothing -> print ""


{-
  runSocket(browser_request):
    The main logic of the proxy.
    Returns: request_response
-}
runSocket :: B.ByteString -> IO (Maybe B.ByteString)
runSocket msg = do
  print msg
  let reqType = getRequestType msg
  url <- getRequestURL msg
  print url
  -- Check if the URL in the request is blocked
  blocked <- isBlocked url
  if (blocked)
    then do
      let bResp = blockResponse
      return (Just $ pack ((show bResp) ++ (unpack $ H.rspBody bResp)))
    else do
      -- The URL is not blocked and so we continue
      -- We need to determine which type of request
      -- it is (e.g. GET, HEAD, etc.)
      case reqType of
        "CONNECT" -> do
          -- Used for https requests
          connMsg <- handleConnect url
          return connMsg
        "NULL" -> do
          -- Could not identify which type of request it is
          return Nothing
        otherwise -> do
          -- http Requests
          -- Only ask for the header first
          headReq <- getHeadRequest url
          manager <- Co.newManager Co.tlsManagerSettings
          headers <- Co.httpLbs headReq manager
          -- Use the header to determine if we can load the
          -- page from cached data
          cacheCheck <- handleCache reqType url headers
          case cacheCheck of
            Just rsp -> do
              -- Load from cached data
              return $ Just rsp
            Nothing -> do
              -- No cached data found, continue with request
              request <- handleRequest reqType url
              response <- Co.httpLbs request manager
              return (Just (pack . parseHTML $ Co.responseBody response))

{-
  getRequestURL(browser_request):
    A helper function to extract the URL from the browser request.
    Returns: request_url
-}
getRequestURL :: B.ByteString -> IO String
getRequestURL req = do
  if (length (splitOn " " (head (splitOn "\n" $ unpack req))) > 1 )
    then return $ (splitOn " " (head (splitOn "\n" $ unpack req))) !! 1
    else return ""

{-
  parseHTML(response):
    A helper function to parse the bytestring response into correct
    HTML syntax
    Returns: webpage_html
-}
parseHTML :: Data.ByteString.Lazy.Internal.ByteString -> String
parseHTML input = htmlResp4 where
  htmlResp0 = init $ tail (show input)
  htmlResp1 = intercalate "\n" (splitOn "\\n" htmlResp0)
  htmlResp2 = intercalate "\"" (splitOn "\\\"" htmlResp1)
  htmlResp3 = intercalate "\r" (splitOn "\\r" htmlResp2)
  htmlResp4 = intercalate "\t" (splitOn "\\t" htmlResp3)

{-
  getRequestType(browser_request):
    A helper function to extract the request type from the browser
    request.
    Returns: request_type
-}
getRequestType :: B.ByteString -> String
getRequestType req = case (take 2 . tail $ show req) of
  "GE" -> "GET"
  "PO" -> "POST"
  "HE" -> "HEAD"
  "PU" -> "PUT"
  "DE" -> "DELETE"
  "OP" -> "OPTIONS"
  "CO" -> "CONNECT"
  otherwise -> "NULL"

-------------------
-- HTTP REQUESTS --
-------------------

{-
  handleRequest(request_type, request_url):
    Creates a correctly formatted request.
    Returns: http_request
-}
handleRequest :: String -> String -> IO Co.Request
handleRequest rType rURL = do
  let sUrl = splitOn ":" rURL
  (sepUrl, rPort) <- if ((head sUrl /= "http") && (head sUrl /= "https"))
    then return (head sUrl, (read (last sUrl)) :: Int)
    else return (rURL, -1)
  request <- Co.parseRequest (rType ++ " " ++ sepUrl)
  if (rPort == -1)
    then return request
    else return request{Co.port = rPort}

--------------------
-- HTTPS REQUESTS --
--------------------

{-
  handleConnect(request_url):
    Handles CONNECT http requests. Uses TLS to connect to secured
    webpages.
    Returns: request_response
-}
handleConnect :: String -> IO (Maybe B.ByteString)
handleConnect connectUrl = do
  let splitUrl = splitOn ":" connectUrl
  let connectHost = head splitUrl
  let connectPort = last splitUrl
  req <- Co.parseRequest $ "https://" ++ connectHost
  print req
  let settings = Co.mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- Co.newManager settings
  res <- Co.httpLbs req manager
  print res
  return (Just (pack $ parseHTML (Co.responseBody res)))

-------------------
-- BLOCKING URLS --
-------------------

{-
  blockResponse:
    Produces the http response when a URL is blocked.
    Returns: blocked_response
-}
blockResponse :: H.Response B.ByteString
blockResponse = H.Response (4,0,3) "Forbidden" [] blockPageHTML

{-
  blockPageHTML:
    Helper function that produces the HTML for the browser
    when a URL is blocked.
    Returns: blocked_html
-}
blockPageHTML :: B.ByteString
blockPageHTML = pack "<!DOCTYPE HTML PUBLIC><html><head><title>403 Forbidden</title></head><body><h1>Forbidden</h1><p>The proxy has blocked this site</p></body></html>"

{-
  blockSite(url):
    Adds a URL to the list of blocked URLs.
-}
blockSite :: String -> IO ()
blockSite newSite = do
  contents <- readFile "blockSites.txt"
  let siteList = read contents :: [String]
  tmp_handle <- openFile "blockSites.temp" ReadWriteMode
  hPutStr tmp_handle (show (siteList ++ [newSite]))
  hClose tmp_handle
  copyFile "blockSites.temp" "blockSites.txt"
  removeFile "blockSites.temp"

{-
  unblockSite(url):
    Removes a URL from the list of blocked URLs.
-}
unblockSite :: String -> IO ()
unblockSite site = do
  contents <- readFile "blockSites.txt"
  let siteList = read contents :: [String]
  when (site `elem` siteList) $ do
    tmp_handle <- openFile "blockSites.temp" ReadWriteMode
    hPutStr tmp_handle (show (siteList \\ [site]))
    hClose tmp_handle
    copyFile "blockSites.temp" "blockSites.txt"
    removeFile "blockSites.temp"

{-
  getBlockedSites:
    Gets the list of blocked URLs.
    Returns: [blocked_urls]
-}
getBlockedSites :: IO [String]
getBlockedSites = do
  contents <- readFile "blockSites.txt"
  let siteList = read contents :: [String]
  return siteList

{-
  isBlocked(url):
    Checks if a URL is in the list of blocked URLs.
    Returns: blocked
-}
isBlocked :: String -> IO Bool
isBlocked site = do
  blockedSites <- getBlockedSites
  return (site `elem` blockedSites)

-------------
-- CACHING --
-------------

{-
  handleCache(request_type, request_url, head_resposne)
    Compares the headers with cached data to determine if a
    webpage can be loaded from cached data.
    Return: request_data_either_cached_or_fresh
-}
handleCache :: String -> String -> Co.Response ByteString -> IO (Maybe B.ByteString)
handleCache reqType url resp = do
  -- These files cannot be cached
  if (".css" `isInfixOf` url || ".js" `isInfixOf` url || ".svg" `isInfixOf` url)
    then return Nothing
    else do
      -- Check if the HEAD response conatins a last modified header
      let respHeads = Co.responseHeaders resp
      if (hLastModified `elem` (map fst respHeads))
        then do
          -- Extract the Last-Modifed field from the HEAD response
          let (_, lastMod) = head $ filter (\x -> fst x == hLastModified) respHeads
          let cacheName = getCacheFileName url
          -- Check to see if the cached data for the request URL exists
          existsCheck <- doesFileExist ("caches/" ++ cacheName ++ ".cache")
          if (existsCheck)
            then do
              -- Extract the Last-Modified field from the cached data
              fileContents <- readFile ("caches/" ++ cacheName ++ ".cache")
              let c_resp = head $ splitOn "\n" fileContents
              let cachedModTime = head $ splitOn "\")" (last $ splitOn ("Last-Modified\",\"") fileContents)
              -- Compare request Last-Modified time and cached Last-Modified time
              if (timeLT cachedModTime (show lastMod))
                then do
                  -- The cached data is out of date.
                  -- Carry out request and update cache.
                  manager <- Co.newManager Co.tlsManagerSettings
                  request <- handleRequest reqType url
                  response <- Co.httpLbs request manager
                  f_handle <- openFile (cacheName ++ ".tempcache") WriteMode
                  hPutStr f_handle (show response)
                  hClose f_handle
                  copyFile (cacheName ++ ".tempcache") ("caches/" ++ cacheName ++ ".cache")
                  removeFile (cacheName ++ ".tempcache")
                  return (Just (pack . parseHTML $ Co.responseBody response))
                else do
                  -- Cache is in date. Return cached data
                  putStrLn "\nRETURNING FROM CACHED DATA\n"
                  return $ Just (pack fileContents)
            else do
              -- Cached data does not exist.
              -- Carry out request and cache response
              manager <- Co.newManager Co.tlsManagerSettings
              request <- handleRequest reqType url
              response <- Co.httpLbs request manager
              f_handle <- openFile (cacheName ++ ".tempcache") WriteMode
              hPutStr f_handle (show response)
              hClose f_handle
              copyFile (cacheName ++ ".tempcache") ("caches/" ++ cacheName ++ ".cache")
              removeFile (cacheName ++ ".tempcache")
              return (Just (pack . parseHTML $ Co.responseBody response))
        -- The request HEAD headers does not contain a Last-Modifed header.
        -- Return nothing
        else return Nothing

{-
  getHeadRequest(request_url):
    Create HEAD request.
    Returns: head_request
-}
getHeadRequest :: String -> IO Co.Request
getHeadRequest rURL = do
  request <- Co.parseRequest ("HEAD " ++ rURL)
  return request

{-
  timeLT(t0, t1):
    Checks to see if t0 <= t1
    Returns: (t0 <= t1)
-}
timeLT :: String -> String -> Bool
timeLT t0 t1 = let
  (d0, m0, y0, s0, mn0, h0) = getTime t0
  (d1, m1, y1, s1, mn1, h1) = getTime t1
  result = (y0 <= y1) && (m0 <= m1) && (d0 <= d1) && (h0 <= h1) && (mn0 <= mn1) && (s0 < s1)
  in result

{-
  getTime(timestamp):
    Helper function that parses a timestamp into a set of integers
    Returns: time_in_ints
-}
getTime :: String -> DateTime
getTime input = let
  sepTime = splitOn " " input
  day = read (sepTime !! 1) :: Int
  month = convertMonth $ sepTime !! 2
  year = read (sepTime !! 3) :: Int
  timeCode = splitOn ":" (sepTime !! 4)
  seconds = read (head timeCode) :: Int
  minutes = read (timeCode !! 1) :: Int
  hours = read (timeCode !! 2) :: Int
  in (day, month, year, seconds, minutes, hours)

{-
  convertMonth(month_str):
    Returns: month_as_int
-}
convertMonth :: String -> Int
convertMonth name
  | name == "Jan" = 1
  | name == "Feb" = 2
  | name == "Mar" = 3
  | name == "Apr" = 4
  | name == "May" = 5
  | name == "Jun" = 6
  | name == "Jul" = 7
  | name == "Aug" = 8
  | name == "Sep" = 9
  | name == "Oct" = 10
  | name == "Nov" = 11
  | name == "Dec" = 12
  | otherwise = 0

{-
  getCacheFileName(url):
    Converts the URL into a correct file name for storage of data.
    Returns: url_file_name
-}
getCacheFileName :: String -> String
getCacheFileName input = finalName where
  editName0 = intercalate "--" (splitOn "/" input)
  editName1 = intercalate "$" (splitOn ":" editName0)
  finalName = intercalate "__" (splitOn "." editName1)

{-
  clearCaches:
    Clears all cached data.
-}
clearCaches :: IO ()
clearCaches = do
  cacheFiles <- listDirectory "caches/"
  mapM_ (\x -> removeFile $ "caches/" ++ x) cacheFiles

------------------------
-- MANAGEMENT CONSOLE --
------------------------

{-
  managementConsole(proxy_id):
    Allows the following commands:
      block <url>: add url to block list
      unblock <url>: removes url from block list
      checkBlock: gets the list of blocked urls
      clearCache: clears all cache data
      stop: stops the proxy
      start: starts the proxy
-}
managementConsole :: Maybe ThreadId -> IO ()
managementConsole proxyId = do
  input_ <- runInputT defaultSettings (getInputLine "")
  case input_ of
    Nothing -> managementConsole proxyId
    Just input -> do
      let iWords = words input
      case (head iWords) of
        "block" -> do
          if (length iWords /= 2)
            then do
              putStrLn "\"block\" takes only one argument"
              managementConsole proxyId
            else do
              blockSite (iWords !! 1)
              putStrLn $ "Site: " ++ (iWords !! 1) ++ " has been added to the block list"
              managementConsole proxyId
        "unblock" -> do
          if (length iWords /= 2)
            then do
              putStrLn "\"unblock\" takes only one argument"
              managementConsole proxyId
            else do
              blockSite (iWords !! 1)
              putStrLn $ "Site: " ++ (iWords !! 1) ++ " has been removed from the block list"
              managementConsole proxyId
        "checkBlock" -> do
          if (length iWords /= 1)
            then do
              putStrLn "\"checkBlock\" takes no arguments"
              managementConsole proxyId
            else do
              putStrLn "Blocked sites: "
              sites <- getBlockedSites
              mapM_ print sites
              managementConsole proxyId
        "clearCache" -> do
          if (length iWords /= 1)
            then do
              putStrLn "\"clearCache\" takes no arguments"
              managementConsole proxyId
            else do
              clearCaches
              putStrLn "Caches cleared"
              managementConsole proxyId
        "stop" -> do
          if (length iWords /= 1)
            then do
              putStrLn "\"stop\" takes no arguments"
              managementConsole proxyId
            else do
              case proxyId of
                Nothing -> do
                  putStrLn "Proxy is not running"
                  managementConsole proxyId
                Just pId -> do
                  killThread pId
                  putStrLn "Proxy stopped"
                  managementConsole Nothing
        "start" -> do
          if (length iWords /= 1)
            then do
              putStrLn "\"start\" takes no arguments"
              managementConsole proxyId
            else do
              case proxyId of
                Just _ -> do
                  putStrLn "Proxy is already running"
                  managementConsole proxyId
                Nothing -> do
                  newPId <- forkIO tcpListener
                  putStrLn "Proxy started"
                  managementConsole (Just newPId)
        otherwise -> do
          putStrLn $ "\"" ++ (head iWords) ++ "\"" ++ " is not a valid command"
          managementConsole proxyId

```
