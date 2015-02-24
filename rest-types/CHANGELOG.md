# Changelog

### 1.13.1

* Add `Applicative` and `Monad` instances for `Reason`.
* Add `Eq`, `Ord` and `Read` instances for `Void`.

## 1.13

* Change the type of `Reason_` from `Reason ()` to `Reason Void`
* Remove the no longer needed `ToResponseCode ()` instance
* Removed `Error` instances for `DataError`, `Reason e`, and `SomeReason`.

## 1.12

* Add `method` field to `Resource`, allowing you to use different
  methods than GET in a multi-action.
* Add `Method` data type.

#### 1.11.1.1

* Return a 405 for unsupported methods.

### 1.11.1

* Moved `Range` from `rest-core` to `Rest.Types.Range`.

## 1.11

* Added `Rest.Types.Error.ToResponseCode` for getting the response codes of errors.
* Removed `responseCode` from `Rest.Types.Error.DomainReason`, use `toResponseCode` instead.
* Added `Rest.Types.Info`, moved from `rest-core`.

### 1.10.2

* Add `Functor`, `Foldable` and `Traversable` instances for the types
  in `Rest.Types.Error`.

### 1.10.1

* Added `ShowUrl` instance for strict and lazy `Text`.

#### 1.10.0.3

* Use `JSONSchema` `Any` type instead of `Choice []`.

#### 1.10.0.2

* Use `rest-stringmap == 0.2.*`.

#### 1.10.0.1

* Allow `mtl == 2.2.*`.

## 1.10

* Removed `Rest.Container.StringMap` in favor of `rest-stringmap`. `KeyValues` is now an alias for `Rest.StringMap.HashMap.Strict String Value`.
