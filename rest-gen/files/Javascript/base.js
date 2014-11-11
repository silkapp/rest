// in nodejs, global is an object
// in normal browser environment, global is undefined
// in webpack, global is set to window
var isNodeJs   = typeof global !== "undefined" && global.window === undefined;
var isCommonJs = typeof module === "object" && module && typeof module.exports === "object";

// require, that the nodejs will handle, but will remain not processed by webpack and the like
var obfuscatedRequire = function (moduleName)
{
  return module["r" + "equire"](moduleName);
}

var $apinamespace$ =
  function (url, secureUrl, modifyRequest)
  {
    var self = this;
    var postfix          = '/v' + this.version + '/';
    var contextUrl       = url + postfix;
    var secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;

    this.cookieJar = isNodeJs ? obfuscatedRequire('request').jar() : undefined;

    if(!modifyRequest) modifyRequest = function(req) { return req; };

    var finalModifyRequest = function(req)
    {
      if (isNodeJs) req.jar = self.cookieJar;
      return modifyRequest(req);
    }

    $apinamespace$.setContext(this, contextUrl, secureContextUrl, finalModifyRequest);
  };

var jq;
if (isNodeJs)
{
  // Export as Node module.
  module.exports = $apinamespace$;

  $apinamespace$.ajaxCall = nodeRequest;
}
else
{
  if (isCommonJs) {
    // Export as CommonJs
    module.exports = $apinamespace$;
    jq = require("jquery");
  } else if (typeof define === "function" && define.amd) {
    // Export as AMD.
    define("$apinamespace$", [], function () { return $apinamespace$; });
    jq = $dollar$;
  } else {
    // Export as global.
    window.$apinamespace$ = $apinamespace$;
    jq = $dollar$;
  }

  $apinamespace$.ajaxCall = jQueryRequest;
}

$apinamespace$.addObject = function (obj1, obj2)
{
  for (var fld in obj2)
    obj1[fld] = obj2[fld];
};

$apinamespace$.defaultAjaxOptions = {};
$apinamespace$.defaultHeaders = {};

function jQueryRequest (method, url, params, success, error, contentType, acceptHeader, data, callOpts, modifyRequest)
{
  var q = window.Q || function (a) { return a };

  var headers = jq.extend(true, {}, $apinamespace$.defaultHeaders);
  $apinamespace$.addObject(headers, { Accept : acceptHeader });

  var callData =
    { type        : method
    , url         : url + (params ? '?' + jq.param(params) : '')
    , cache       : false
    , success     : success || function () {}
    , error       : error || function () {}
    , contentType : contentType
    , headers     : headers
    , xhrFields   : { withCredentials: true }
    , data        : data || []
    };

  callData = modifyRequest(callData);

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return q(jq.ajax(callData));
}

function nodeRequest (method, url, params, onSuccess, onError, contentType, acceptHeader, data, callOpts, modifyRequest)
{
  var allParams = {};
  $apinamespace$.addObject(allParams, params);

  if (method === "GET" || method === "HEAD")
    // Avoid cached API responses.
    allParams._ = Date.now();

  var headers = { "Content-type" : contentType
                , "Accept"       : acceptHeader
                };

  $apinamespace$.addObject(headers, $apinamespace$.defaultHeaders);

  var callData =
    { url     : url
    , qs      : allParams
    , method  : method
    , headers : headers
    };

  if (data) callData.body = data;

  callData = modifyRequest(callData);

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return require("q").Promise(function (resolve, reject)
  {
    obfuscatedRequire("request")(callData, callback);

    function callback (error, message, body)
    {
      if (message && message.statusCode >= 200 && message.statusCode < 300)
      {
        var parsedResponse = parse(body);
        onSuccess && onSuccess(parsedResponse, message);
        resolve(parsedResponse)
      }
      else
      {
        if (!error)
        {
          error = new Error("HTTP request error");
          error.statusCode = message.statusCode;
          error.responseBody = body;
        }

        error.response = parse(body);

        if (onError)
          onError(error);

        reject(error);
      }
    }
  });

  function parse (response)
  {
    if (acceptHeader.split(";").indexOf('text/json') >= 0)
    {
      var r = response;
      try
      {
        r = JSON.parse(response);
      }
      catch (e)
      {
        return r;
      }
      return r;
    }
    else return response;
  }
}

$apinamespace$.setContext =
  function (obj, url, secureUrl, modifyRequest)
  {
    obj.contextUrl = url;
    obj.secureContextUrl = secureUrl;
    obj.modifyRequest = modifyRequest;
    for (var fld in obj)
    {
      if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
      {
        var postfix = fld.replace(/([a-z0-9])([A-Z])/g, '$dollar$1-$dollar$2').toLowerCase() + '/';
        $apinamespace$.setContext(obj[fld], url + postfix, secureUrl + postfix, modifyRequest);
      }
    }
  };
