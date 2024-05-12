# MTL works

```
$ stack run -- mtl
```

```
$ curl localhost:3000/stream
{"itemId":1,"itemText":"Drew"}
{"itemId":2,"itemText":"Drew"}
{"itemId":3,"itemText":"Drew"}
{"itemId":4,"itemText":"Drew"}
{"itemId":5,"itemText":"Drew"}
{"itemId":6,"itemText":"Drew"}
{"itemId":7,"itemText":"Drew"}
{"itemId":8,"itemText":"Drew"}
{"itemId":9,"itemText":"Drew"}
{"itemId":10,"itemText":"Drew"}
```

# Effectful fails

```
$ stack run -- effectful
version (3) /= storageVersion (0)
CallStack (from HasCallStack):
  error, called at src/Effectful/Internal/Env.hs:358:5 in effectful-core-2.3.0.1-8gRe1gsiGtKADksLppQ43Q:Effectful.Internal.Env
```

```
$ curl localhost:3000/stream
curl: (52) Empty reply from server
```
