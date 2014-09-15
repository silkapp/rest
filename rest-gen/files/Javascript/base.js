var $apinamespace$ =
  function (url, secureUrl)
  {
    var postfix          = '/v' + this.version + '/';
    var contextUrl       = url + postfix;
    var secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;

    $apinamespace$.setContext(this, contextUrl, secureContextUrl);
  };

if (typeof module === "object" && module && typeof module.exports === "object")
{
  // Export as Node module.
  module.exports = $apinamespace$;

  $apinamespace$.ajaxCall = nodeRequest;
}
else
{
  if (typeof define === "function" && define.amd)
    // Export as AMD.
    define("$apinamespace$", [], function () { return $apinamespace$; });

  else
    // Export as global.
    window.$apinamespace$ = $apinamespace$;

  $apinamespace$.ajaxCall = jQueryRequest;
}

$apinamespace$.addObject = function (obj1, obj2)
{
  for (var fld in obj2)
    obj1[fld] = obj2[fld];
};

$apinamespace$.defaultAjaxOptions = {};
$apinamespace$.defaultHeaders = {};

function jQueryRequest (method, url, params, success, error, contentType, acceptHeader, data, callOpts)
{
  var q = window.Q || function (a) { return a };

  var headers = $dollar$.extend(true, {}, $apinamespace$.defaultHeaders);
  $apinamespace$.addObject(headers, { Accept : acceptHeader });

  var callData =
    { type: method
    , url: url + (params ? '?' + $dollar$.param(params) : '')
    , cache: false
    , success: success || function () {}
    , error: error || function () {}
    , contentType: contentType
    , headers: headers
    , xhrFields: { withCredentials: true }
    , data: data || []
    };

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return q($dollar$.ajax(callData));
}

function nodeRequest (method, url, params, onSuccess, onError, contentType, dataType, data, callOpts)
{
  var allParams = {};
  $apinamespace$.addObject(allParams, params);

  if (method === "GET" || method === "HEAD")
    // Avoid cached API responses.
    allParams._ = Date.now();

  var headers = { "Content-type": contentType };

  if (dataType === 'json')
    headers.Accept = 'text/json';
  else if (dataType === 'xml')
    headers.Accept = 'text/xml';

  $apinamespace$.addObject(headers, $apinamespace$.defaultHeaders);

  var callData =
    { url     : url
    , qs      : allParams
    , method  : method
    , headers : headers
    };

  if (data) callData.body = data;

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return require("q").Promise(function (resolve, reject)
  {
    require("request")(callData, callback);

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

        if (onError)
          onError(error);

        reject(error);
      }
    }
  });

  function parse (response)
  {
    if (dataType === 'json')
      return JSON.parse(response);
    else return response;
  }
}

$apinamespace$.setContext =
  function (obj, url, secureUrl)
  {
    obj.contextUrl = url;
    obj.secureContextUrl = secureUrl;
    for (var fld in obj)
    {
      if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
      {
        var postfix = fld.replace(/([a-z0-9])([A-Z])/g, '$dollar$1-$dollar$2').toLowerCase() + '/';
        $apinamespace$.setContext(obj[fld], url + postfix, secureUrl + postfix);
      }
    }
  };
