# Changelog

#### 0.34.0.2

* Allow utf8-string 1.

#### 0.34.0.1

* Fix for base 4.8/GHC 7.10.

## 0.34

* Allow setting the method in the requests in a top level multi
  handler.
* The `getMethod` `Rest` class function now returns a `Maybe`.
* The `route` method in `Rest.Driver.Routing` now takes a `Maybe
  Method`.
* The `Method` type was removed from `Rest.Driver.Types` and moved to
  rest-types' `Rest.Types.Method`.

#### 0.33.2

* Allow top level multi handler as a POST as well as a GET.
  Technically, you're not allowed to vary the response based on the
  body of a GET. Also, in some frameworks (e.g. jQuery) it isn't
  possible to set the body of a GET.

#### 0.33.1.2

* Typo fixes in documentation.

#### 0.33.1.1

* Declare correct error return type for derived multi handlers. These
  were declared as `Reason (Reason e)`, now they are `Reason ()`. This
  generated confusing documentation.

### 0.33.1

* Move `Range` from `Rest.Handler` into `rest-types`. Still re-exported.

## 0.33

* Added a `Rest.Types.Error.ToResponseCode` constraint to `jsonE`, `xmlE`, and `xmlJsonE`.
* Changed `domainReason` to have a `ToResponseCode` constraint instead of an explicit argument.
* Added `Rest.ShowUrl` re-exporting `ShowUrl` from `rest-types`.
* `application/*` and `application/octet-stream` accept headers now match file outputs.

#### 0.32.0.2
* Allow random 1.1.*

#### 0.32.0.1
* Fix bug in Chrome when serving files with commas in name.

## 0.32

* Add `addHeader` dictionary combinator, to extend instead of replace
  the header dictionaries. For this, a constructor `TwoHeaders` was
  added to `Header`.
* Relax the types of `mkListing` and `mkOrderedListing` to allow
  parameters and headers.

### 0.31.1

* Expose `Rest.Driver.Routing.splitUriString`.
* Make test cases compile again.

## 0.31

* Schema: `action` has been renamed to `static` since it is tied to `statics` and to disambiguate it from the unrelated `actions`.

#### 0.30.0.3

* Use `json-schema 0.5.*`

#### 0.30.0.2

* Use `rest-stringmap == 0.2.*`

#### 0.30.0.1

* Allow `mtl == 2.2.*` and `transformers == 0.4.*`

## 0.30

* Use `Content-Disposition` to provide filenames for file responses.
  This slightly changes the semantics of `FileO`: what used to be
  interpreted as the file extension is now used for the whole file name.

* `Rest.Types.Container.StringMap` Has been replaced by `rest-stringmap`. This
  changes the XML serialization format of multi part messages, the old format
  was `<map><key>k</key>v[...]</map>` and the new one is
  `<map><value key="k">v</value>[...]</map>`.

## 0.29

* Add multi-delete handler. It is used on a DELETE to
  `/<resource>/<id>/` and is derived from the single delete handler.
* Don't put `Cache-Control: private` header on served files. This way
  they can be cached by public proxies, e.g. cloudfront.
* Add `Show` instances for `Header`, `Param` and `Dict`.
* Renamed `mkMultiPutHandler` to `mkMultiHandler` in
  `Rest.Driver.Routing`.
* Explicit exports in `Rest.Driver.Routing`, removing a lot of
  private functions from the public interface.
