# Changelog

#### 0.17.0.6

* Fix linking of documentation resources (thanks Jan-Philip Loos)

#### 0.17.0.5

* Allow `aeson 0.9.*`

#### 0.17.0.4

* Allow `rest-core 0.36.*`

#### 0.17.0.3

* Allow `tagged 0.8.*`

#### 0.17.0.2

* Allow `filepath 1.4.*`.

#### 0.17.0.1

* Allow `blaze-html 0.8.*`.

## 0.17.0.0

* Docs: More exhaustive examples of JSON messages showing each possible node in the schema at least once.
* Docs: Make example popup scrollable.

#### 0.16.1.8

* Remove dependency on hslogger.

#### 0.16.1.7

* Allow base 4.8 and HStringTemplate 0.8 for GHC 7.10 compatibility.

#### 0.16.1.6

* Bump `rest-core`.

#### 0.16.1.5

* Haskell: Escape special characters in parameter names

#### 0.16.1.4

* Allow Cabal 1.22.*.

#### 0.16.1.3

* Allow semigroups 0.16.*

#### 0.16.1.2

* JavaScript: Add support for webpack (and any other preprocessor that tries to import all `require` function calls)

#### 0.16.1.1

* Haskell: Fixes a bug where client generation for end points without defined output or errors would default to parsing errors as XML even though the accept header was text/json. It now parses the response as json.

### 0.16.1

##### JavaScript

* When instantiating API object you can now optionally pass a `modifyRequest` argument of type `RequestObject -> RequestObject` which is called before every request.

* NodeJS: The `cookieJar` instance member is no longer available on sub resources, it can now only be accessed from the top-level api object. This is a breaking change for generated clients. It was a bad idea to use this to begin with since the cookie jar is shared across the entire api object.

#### 0.16.0.4

* JavaScript: Minor simplification in the template.

#### 0.16.0.3

* JavaScript/node.js: Each instance of a client now uses its own cookie jar automatically. You no longer need to depend on the `request` library to use generated clients. If you want access to the cookie jar use `api.cookieJar`.

#### 0.16.0.2

* Allow haskell-src-exts 1.16.*

#### 0.16.0.1

* Fix a bug where the generated Haskell client for a resource with a
  name that was a Haskell keyword would contain invalid code.
* Relax bounds on semigroups.

## 0.16

You don't have to regenerate clients for this release, but it is strongly recommended!

* Accept headers are now sent based on both the output and errors of an endpoint, previously none of the clients would handle an end point with different inputs and outputs properly.

* JavaScript/node.js: Error objects now contain a JSON `response` property if the error response is valid JSON.

#### 0.15.0.3

* Bump `rest-types` and `rest-core`

#### 0.15.0.2

* Allow `json-schema 0.7.*`

#### 0.15.0.1

* JavaScript/node.js: Fixed error handling for `ECONNREFUSED` and other "top level" errors

## 0.15

* JavaScript/node.js: Clients now use and require Q promises
* JavaScript/web: Will now use Q if available, and otherwise return the normal jQuery objects

Bugfixes:
* JavaScript: Fix propagation of URLs to secure end points

### 0.14.2

* Generated JavaScript now also works as a Node module
* JavaScript constructors are now named properly instead of "constrWrapper"
* Now generates valid Ruby code again

### 0.14.1

* Expose modules Rest.Gen.Base.ActionInfo and Rest.Gen.Base.ApiTree
  again.
* Make test cases compile again.

##### Revisions

* Allow `Cabal == 1.16.*`

## 0.14

* Abstracted generated code into rest-client, you should regenerate haskell clients
* haskell code generation is now done using `haskell-src-exts`
* When using module name rewrites their qualification are now also rewritten.
* Add `hs-source-dirs` and `build-depends` when generating cabal files
* Moved `Rest.Gen.Docs.Happstack` to `rest-happstack:Rest.Driver.Happstack.Docs`
* Expose `Rest.Gen.Base`
* Flattened module hierarchy, `Rest.Gen.Haskell.Generate` is now `Rest.Gen.Haskell` etc.

#### 0.13.1.2

* Use `json-schema 0.5.*` and add `showExample` cases for `Map` and `Any`

#### 0.13.1.1

* Allow `attoparsec 0.12.*`

### 0.13.1

* Derive `Eq, Show` for all types in `Rest.Gen.Types`
* Haskell: Nub generated imports

## 0.13

Breaking changes:
* Un-exposes internal modules so we don't have to major bump on every change.
* `gen` Now accepts AST-like types instead of just strings to make it more obvious how to use it, see types in `Rest.Gen.Types`

Bugfixes:
* Make sure Identifiers are always imported when needed. This is a further improvement on the bugfix in rest-gen-0.11.
* rest-gen-0.12	did not always take arguments in generated methods into account, so the renamed qualification has been reverted for now.

## 0.12

* Haskell: Module rewrites such as `Data.Text.Internal` -> `Data.Text` now produces qualified imports `import qualified Data.Text as Data.Text` instead of `import qualified Data.Text as Data.Text.Lazy`. This prevents building against different versions of the same package that may have moved the internal module (as is the case with `text`) from generating different clients.

## 0.11

* Bugfix: Haskell: Resources without a getter now generate identifier arguments for other end points
* Shuffles some internal (but exposed) functions around

#### 0.10.0.4

* Drops `aeson-utils` dependency in favor of `scientific >= 0.3.2`

#### 0.10.0.3

* Allow `mtl == 2.2.*`

#### 0.10.0.1

* Bump `Cabal` upper bound to `< 1.22`

## 0.10

* Generate documentation, Javascript and Haskell code for multi-delete
  handlers. The name of the generated functions is `removeMany` or
  `removeManyBy<id>`.
* Escape reserved names in Haskell code generation.
