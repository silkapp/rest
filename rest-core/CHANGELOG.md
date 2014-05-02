# Changelog

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
