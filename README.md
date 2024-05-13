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

# Effectful ~~fails~~ works

Effectful now works thanks to `withCloneEnv` helper! See [this
issue](https://github.com/haskell-effectful/effectful/issues/219).

```
$ stack run -- effectful
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
