var $apinamespace$ =
  function (url, secureUrl)
  {
    var postfix = '/v' + this.version + '/';
    var sUrl = url + postfix;
    $apinamespace$.setContext(this, sUrl);
    this.secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;
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

function jQueryRequest (method, url, params, success, error, contentType, dataType, data, callOpts)
{
  var callData =
    { type: method
    , url: url + (params ? '?' + $dollar$.param(params) : '')
    , cache: false
    , success: success || function () {}
    , error: error || function () {}
    , contentType: contentType
    , dataType: dataType
    , xhrFields: { withCredentials: true }
    , data: data || []
    };

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return $dollar$.ajax(callData);
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

  var callData =
    { url     : url
    , qs      : allParams
    , method  : method
    , headers : headers
    };

  if (data) callData.body = data;

  $apinamespace$.addObject(callData, $apinamespace$.defaultAjaxOptions);
  $apinamespace$.addObject(callData, callOpts);

  return require("request")(callData, callback);

  function callback (error, message, response)
  {
    var parsedResponse = parse(response);
    if (message.statusCode >= 200 && message.statusCode < 300)
      onSuccess && onSuccess(parsedResponse, message);
    else if (onError)
      onError(error, parsedResponse, message);
  }

  function parse (response)
  {
    if (dataType === 'json')
      return JSON.parse(response);
    else return response;
  }
}

$apinamespace$.setContext =
  function (obj, url)
  {
    obj.contextUrl = url;
    for (var fld in obj)
    {
      if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
      {
        var newUrl = url + fld.replace(/([a-z0-9])([A-Z])/g, '$dollar$1-$dollar$2').toLowerCase() + '/'
        $apinamespace$.setContext(obj[fld], newUrl);
      }
    }
  };
