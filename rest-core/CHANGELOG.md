# Changelog

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
