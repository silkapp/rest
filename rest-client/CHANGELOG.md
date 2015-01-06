# Changelog

#### 0.4.0.3

* Bump `rest-core`.

#### 0.4.0.2

* Support `monad-control == 1.0.*`.

#### 0.4.0.1

* Bump `rest-types`

## 0.4

* Moved functionality from rest-gen into this package, you should regenerate haskell clients.

## 0.3.0.0

* Replace runT' -> runT, use runResourceT (runT ...) if you want the old behavior.

* Allow `http-conduit == 2.1.*.

#### 0.2.3.5

* Allow `mtl == 2.2.*`.

#### 0.2.3.4

* Widen dependency on aeson-utils to `>= 0.2.1 && < 0.3`.
