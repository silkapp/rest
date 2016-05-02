# Changelog

#### 0.3.1.1

* Allow rest-core-0.39.

### 0.3.1

* Generalize type of `apiToHandler` to allow transformer stacks on top
  of `ServerPartT`. Thanks to ariep.

## 0.3

* Breaking: `Applicative` and `Monad` constraint on `m` in
  `apiToHandler'`. See `rest-core-0.37` for more details.

#### 0.2.10.8

* Allow `rest-core 0.36.*`
* Allow `rest-gen 0.18.*`

#### 0.2.10.7

* Bump `rest-core` and `rest-gen` upper bounds.

#### 0.2.10.6

* Allow `utf8-string 1.0.*`.

#### 0.2.10.5

* Use new version of `rest-core`.

#### 0.2.10.4

* Allow `happstack-server == 7.4.*`

#### 0.2.10.3

* Bump `rest-types` and `rest-core`

#### 0.2.10.2

* Allow `rest-gen == 0.15.*`

#### 0.2.10

* Introduced `Rest.Driver.Happstack.Docs` module that was moved from `rest-gen:Rest.Gen.Docs.Happstack`

#### 0.2.9.9

* Allow `mtl == 2.2.*` and `transformers == 0.4.*`
