Dice as a Service
=================
*On your way to an epic quest but suddenly realizing you forgot your precious
plastic icosahedrons at home? Annoyed at the lack of 47-sided dice at your local
game store? Fear not! Start your own dice service today!*

The service listens to to http GET requests and can answer in either plain text,
json or html depending on the accept-header of the request. The dice service
parses the url and expects a string representing a dice roll (ie. 2d20+4 or 5d6)
as the only parameter.

[Try it!](http://dice.cryon.se/)

Usage (client)
==============
```
~ $ curl localhost/2d20+4 -H "accept: text/plain"
25

~ $ curl localhost/5d6 -H "accept: application/json" | pretty_json
{
   "diceroll" : {
      "number" : 5,
      "sides" : 6,
      "modifier" : 0
   },
   "result" : 14
}
```
Usage (server)
==============
```
~ $ dice --help
Starts a service that listens for incoming HTTP GET requests, parses the
argument and returns the result in either plain text, json or html

Usage: dice [-p|--port PORT]
  Dice as a service (DaaS)

Available options:
  -h,--help                Show this help text
  -p,--port PORT           Port that service listens to (defaults to 80)

~ $ dice --port 8080 >> dice.log
```
Build
=====
A simple <code>cabal install</code> should be enough as long as you have a
fairly new ghc on your system. I highly recommend building it in a sandbox!

Also
====
I created this project for the sole purpose of learning more about Haskell. I
don't expect it to be useful.

If you think this sucks there's another service I recommend for you
[http://foaas.com/](http://foaas.com/)!

Background tile in HTML response by [Brant
Wilson](http://lostandtaken.com/blog/2012/1/4/30-free-seamless-background-textures.html)
