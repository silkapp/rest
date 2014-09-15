(function (window) {

var RestexampleApi =
  function (url, secureUrl)
  {
    var postfix          = '/v' + this.version + '/';
    var contextUrl       = url + postfix;
    var secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;

    RestexampleApi.setContext(this, contextUrl, secureContextUrl);
  };

if (typeof module === "object" && module && typeof module.exports === "object")
{
  // Export as Node module.
  module.exports = RestexampleApi;

  RestexampleApi.ajaxCall = nodeRequest;
}
else
{
  if (typeof define === "function" && define.amd)
    // Export as AMD.
    define("RestexampleApi", [], function () { return RestexampleApi; });

  else
    // Export as global.
    window.RestexampleApi = RestexampleApi;

  RestexampleApi.ajaxCall = jQueryRequest;
}

RestexampleApi.addObject = function (obj1, obj2)
{
  for (var fld in obj2)
    obj1[fld] = obj2[fld];
};

RestexampleApi.defaultAjaxOptions = {};
RestexampleApi.defaultHeaders = {};

function jQueryRequest (method, url, params, success, error, contentType, dataType, data, callOpts)
{
  var q = window.Q || function (a) { return a };

  var callData =
    { type: method
    , url: url + (params ? '?' + $.param(params) : '')
    , cache: false
    , success: success || function () {}
    , error: error || function () {}
    , contentType: contentType
    , dataType: dataType
    , xhrFields: { withCredentials: true }
    , data: data || []
    , headers: RestexampleApi.defaultHeaders
    };

  RestexampleApi.addObject(callData, RestexampleApi.defaultAjaxOptions);
  RestexampleApi.addObject(callData, callOpts);

  return q($.ajax(callData));
}

function nodeRequest (method, url, params, onSuccess, onError, contentType, dataType, data, callOpts)
{
  var allParams = {};
  RestexampleApi.addObject(allParams, params);

  if (method === "GET" || method === "HEAD")
    // Avoid cached API responses.
    allParams._ = Date.now();

  var headers = { "Content-type": contentType };

  if (dataType === 'json')
    headers.Accept = 'text/json';
  else if (dataType === 'xml')
    headers.Accept = 'text/xml';

  RestexampleApi.addObject(headers, RestexampleApi.defaultHeaders);

  var callData =
    { url     : url
    , qs      : allParams
    , method  : method
    , headers : headers
    };

  if (data) callData.body = data;

  RestexampleApi.addObject(callData, RestexampleApi.defaultAjaxOptions);
  RestexampleApi.addObject(callData, callOpts);

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

RestexampleApi.setContext =
  function (obj, url, secureUrl)
  {
    obj.contextUrl = url;
    obj.secureContextUrl = secureUrl;
    for (var fld in obj)
    {
      if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
      {
        var postfix = fld.replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase() + '/';
        RestexampleApi.setContext(obj[fld], url + postfix, secureUrl + postfix);
      }
    }
  };RestexampleApi.prototype.version = "1.0.0";
RestexampleApi.prototype.Post =
  function Post (url, secureUrl)
  {
    if (this instanceof Post)
    {
      RestexampleApi.setContext(this, url, secureUrl);
    }
    else
    {
      return Post.access(url, secureUrl);
    }
  };
RestexampleApi.prototype.Post.apiObjectType = "resourceDir";
RestexampleApi.prototype.Post.byId =
  function (integer)
  {
    var postfix = 'id/' + encodeURIComponent(integer) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "json", undefined, callOpts);
      };
    return accessor;
  };
RestexampleApi.prototype.Post.latest =
  function ()
  {
    var postfix = 'latest/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "json", undefined, callOpts);
      };
    return accessor;
  };
RestexampleApi.prototype.Post.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "json", undefined, callOpts);
  };
RestexampleApi.prototype.Post.removeManyById =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + 'id/', params, success, error, "text/json", "json", JSON.stringify(json), callOpts);
  };
RestexampleApi.prototype.Post.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "json", JSON.stringify(json), callOpts);
  };
RestexampleApi.prototype.Post.prototype.remove =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + '', params, success, error, "text/plain", "text", undefined, callOpts);
  };
RestexampleApi.prototype.Post.prototype.Comment =
  function Comment (url, secureUrl)
  {
    if (this instanceof Comment)
    {
      RestexampleApi.setContext(this, url, secureUrl);
    }
    else
    {
      return Comment.access(url, secureUrl);
    }
  };
RestexampleApi.prototype.Post.prototype.Comment.apiObjectType = "resourceDir";
RestexampleApi.prototype.Post.prototype.Comment.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "json", undefined, callOpts);
  };
RestexampleApi.prototype.Post.prototype.Comment.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "json", JSON.stringify(json), callOpts);
  };
RestexampleApi.prototype.Test =
  function Test (url, secureUrl)
  {
    if (this instanceof Test)
    {
      RestexampleApi.setContext(this, url, secureUrl);
    }
    else
    {
      return Test.access(url, secureUrl);
    }
  };
RestexampleApi.prototype.Test.apiObjectType = "resourceDir";
RestexampleApi.prototype.Test.prototype.onlyError =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'onlyError/', params, success, error, "text/plain", "text", undefined, callOpts);
  };
RestexampleApi.prototype.Test.prototype.differentFormats =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'differentFormats/', params, success, error, "text/plain", "xml", text, callOpts);
  };
RestexampleApi.prototype.Test.prototype.intersectedFormats =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'intersectedFormats/', params, success, error, "text/plain", "json", text, callOpts);
  };
RestexampleApi.prototype.Test.prototype.intersectedFormats2 =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'intersectedFormats2/', params, success, error, "text/plain", "json", text, callOpts);
  };
RestexampleApi.prototype.User =
  function User (url, secureUrl)
  {
    if (this instanceof User)
    {
      RestexampleApi.setContext(this, url, secureUrl);
    }
    else
    {
      return User.access(url, secureUrl);
    }
  };
RestexampleApi.prototype.User.apiObjectType = "resourceDir";
RestexampleApi.prototype.User.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "json", undefined, callOpts);
  };
RestexampleApi.prototype.User.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "json", JSON.stringify(json), callOpts);
  };

})(this);
