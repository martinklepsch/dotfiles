(require :luarocks.loader)
(local socket (require :socket))
(local bencode (require :bencode))

(local active-requests {})
(var counter 0)
(var session nil)
(var ns nil)
(var prompt "=> ")

(λ count [tbl]
  (var i 0)
  (each [_ (pairs tbl)] (set i (+ 1 i)))
  i)

(λ contains? [tbl item ?n]
  (or (= item (. tbl (or ?n 1)))
      (if (<= (or ?n 1) (# tbl))
          (contains? tbl item (+ 1 (or ?n 1))))))

(λ send [msg conn]
  (print "send" msg)
  (set (msg.id counter) (values counter (+ counter 1)))
  (tset active-requests msg.id true)
  (when (not msg.session)
    (set msg.session session))
  (assert (: conn :send (bencode.encode msg))))

(λ send-input [?session]
  (let [input (io.read)]
    (send {:op :stdin
           :session ?session
           :stdin (if (= nil input) "" input)})))

(λ handler [response]
  (when response.new-session
    (print "Connected.")
    (set session response.new-session))

  (when response.err (print response.err))
  (when response.out (print response.out))
  (when response.value (print response.value))

  (when response.ns
    (set ns response.ns)
    (set prompt (.. response.ns "=> ")))

  (when response.status
    (when (contains? response.status :interrupted)
      (print "Evaluation interrupted.")
      (tset active-requests response.id nil))
    (when (contains? response.status :done)
      (tset active-requests response.id nil))
    (when (contains? response.status :needs-input)
      (send-input response.session)))
  (when (os.getenv "DEBUG")
    (local view (require :fennelview))
    (print "   " (view response) (view active-requests))))

(λ receive [conn]
  (let [(data err part) (: conn :receive "*a")]
    (when (or data (and part (not= part "")))
      (let [(decodeds d-err) (bencode.decode_all (or data part))]
        (if decodeds
            (each [_ decoded (ipairs decodeds)]
              (handler decoded))
            (print (.. "Decoding error: " d-err (or data part) "\n"))))
      true)))

(local [host port] (let [(host port) (: (. arg 1) :match "(.*):(.*)")]
                     (if host
                         [host port]
                         ["localhost" (. arg 1)])))

(print "host" host)
(print "port" port)

(when (not port)
  (: io.stderr :write "USAGE: shevek [HOST:]PORT\n")
  (os.exit 1))

(local conn (let [c (assert (socket.connect host (tonumber port)))]
              (: c :settimeout 0.01)
              (send {:op :clone} c)
              c))

(print "session" session)

(while (not session)
  (receive conn))

(io.write prompt)

((fn repl []
   ;; this is not good; we should use non-blocking read here, but that's Hard
   (print "repl called")
   (let [input (io.read)]
     (when (not input) (os.exit 0))
     (send {:op :eval :code input} conn)
     (while (< 0 (count active-requests))
       (receive conn))
     (io.write prompt)
     (repl))))
