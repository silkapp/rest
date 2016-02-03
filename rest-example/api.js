(function (window) {

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

function RestexampleApi (url, secureUrl, modifyRequest)
{
  var RestexampleApi =
    function (url, secureUrl, modifyRequest)
    {
      var self = this;
      var postfix          = '/v' + this.version + '/';
      var contextUrl       = url + postfix;
      var secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;

      this.cookieJar = isNodeJs ? obfuscatedRequire('request').jar() : undefined;

      if(!modifyRequest) modifyRequest = function (req) { return req; };

      var finalModifyRequest = function (req)
      {
        if (isNodeJs) req.jar = self.cookieJar;
        return modifyRequest(req);
      }

      RestexampleApi.setContext(this, contextUrl, secureContextUrl, finalModifyRequest);
    };

  var jqFun;
  if (isNodeJs)
  {
    RestexampleApi.ajaxCall = nodeRequest;
  }
  else
  {
    if (isCommonJs) {
      jqFun = function () { return require("jquery"); };
    } else if (typeof define === "function" && define.amd) {
      jqFun = function () { return window.$; };
    } else {
      jqFun = function () { return window.$; };
    }

    RestexampleApi.ajaxCall = jQueryRequest;
  }

  RestexampleApi.addObject = function (obj1, obj2)
  {
    for (var fld in obj2)
      obj1[fld] = obj2[fld];
  };

  RestexampleApi.defaultAjaxOptions = {};
  RestexampleApi.defaultHeaders = {};

  function jQueryRequest (method, url, params, success, error, contentType, acceptHeader, data, callOpts, modifyRequest)
  {
    var jq = jqFun();

    var headers = jq.extend(true, {}, RestexampleApi.defaultHeaders);
    RestexampleApi.addObject(headers, { Accept : acceptHeader });

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

    RestexampleApi.addObject(callData, RestexampleApi.defaultAjaxOptions);
    RestexampleApi.addObject(callData, callOpts);

    return jq.ajax(callData);
  }

  function nodeRequest (method, url, params, onSuccess, onError, contentType, acceptHeader, data, callOpts, modifyRequest)
  {
    var allParams = {};
    RestexampleApi.addObject(allParams, params);

    if (method === "GET" || method === "HEAD")
      // Avoid cached API responses.
      allParams._ = Date.now();

    var headers = { "Content-type" : contentType
                  , "Accept"       : acceptHeader
                  };

    RestexampleApi.addObject(headers, RestexampleApi.defaultHeaders);

    var callData =
      { url     : url
      , qs      : allParams
      , method  : method
      , headers : headers
      };

    if (data) callData.body = data;

    callData = modifyRequest(callData);

    RestexampleApi.addObject(callData, RestexampleApi.defaultAjaxOptions);
    RestexampleApi.addObject(callData, callOpts);

    return require("q").Promise(function (resolve, reject)
    {
      obfuscatedRequire("request")(callData, callback);

      function callback (error, message, body)
      {
        if (message && message.statusCode >= 200 && message.statusCode < 300)
        {
          var parsedResponse = parse(body, message.headers);
          onSuccess && onSuccess(parsedResponse, message);
          resolve(parsedResponse)
        }
        else
        {
          if (!error)
          {
            error = new Error("HTTP request error");
            error.statusCode = message && message.statusCode;
            error.responseBody = body;
          }

          error.response = parse(body, message ? message.headers : {});

          if (onError)
            onError(error);

          reject(error);
        }
      }
    });

    function parse (response, headers)
    {
      if (headers["content-type"] && headers["content-type"].split(";").indexOf("application/json") >= 0)
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

  RestexampleApi.setContext =
    function (obj, url, secureUrl, modifyRequest)
    {
      obj.contextUrl = url;
      obj.secureContextUrl = secureUrl;
      obj.modifyRequest = modifyRequest;
      for (var fld in obj)
      {
        if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
        {
          var postfix = fld.replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase() + '/';
          RestexampleApi.setContext(obj[fld], url + postfix, secureUrl + postfix, modifyRequest);
        }
      }
    };
RestexampleApi.prototype.version = "1.0.0";
RestexampleApi.prototype.Post =
  function Post (url, secureUrl, modifyRequest)
  {
    if (this instanceof Post)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Post.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.Post.apiObjectType = "resourceDir";
RestexampleApi.prototype.Post.byId =
  function (integer)
  {
    var postfix = 'id/' + encodeURIComponent(integer) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
RestexampleApi.prototype.Post.latest =
  function ()
  {
    var postfix = 'latest/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
RestexampleApi.prototype.Post.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Post.removeManyById =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + 'id/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Post.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Post.prototype.remove =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Post.prototype.Comment =
  function Comment (url, secureUrl, modifyRequest)
  {
    if (this instanceof Comment)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Comment.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.Post.prototype.Comment.apiObjectType = "resourceDir";
RestexampleApi.prototype.Post.prototype.Comment.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Post.prototype.Comment.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test =
  function Test (url, secureUrl, modifyRequest)
  {
    if (this instanceof Test)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Test.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.Test.apiObjectType = "resourceDir";
RestexampleApi.prototype.Test.prototype.noResponse =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'noResponse/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.onlyError =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'onlyError/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.differentFormats =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'differentFormats/', params, success, error, "text/plain", "text/json,text/xml", text, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.intersectedFormats =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'intersectedFormats/', params, success, error, "text/plain", "text/json", text, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.intersectedFormats2 =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'intersectedFormats2/', params, success, error, "text/plain", "text/xml", text, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.rawXmlIO =
  function (xml, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'rawXmlIO/', params, success, error, "text/xml", "text/xml", xml, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.rawJsonIO =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'rawJsonIO/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.rawJsonAndXmlI =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'rawJsonAndXmlI/', params, success, error, "text/json", "text/plain,text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.rawJsonAndXmlO =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'rawJsonAndXmlO/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.noError =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'noError/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.justStringO =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'justStringO/', params, success, error, "text/plain", "text/plain,text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.preferJson =
  function (text, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'preferJson/', params, success, error, "text/plain", "text/json", text, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.octetStreamOut =
  function (file, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'octetStreamOut/', params, success, error, "application/octet-stream", "text/json,application/octet-stream", file, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.onlyInput =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'onlyInput/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.FooBar =
  function FooBar (url, secureUrl, modifyRequest)
  {
    if (this instanceof FooBar)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return FooBar.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.Test.prototype.FooBar.apiObjectType = "resourceDir";
RestexampleApi.prototype.Test.prototype.FooBar.removeManyById =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + 'id/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.FooBar.prototype.remove =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("DELETE", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.Test.prototype.Import =
  function Import (url, secureUrl, modifyRequest)
  {
    if (this instanceof Import)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Import.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.Test.prototype.Import.apiObjectType = "resourceDir";
RestexampleApi.prototype.Test.prototype.Import.byIt =
  function (string)
  {
    var postfix = 'it/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
RestexampleApi.prototype.Test.prototype.Import.prototype.do_ =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + 'do/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.User =
  function User (url, secureUrl, modifyRequest)
  {
    if (this instanceof User)
    {
      RestexampleApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return User.access(url, secureUrl, modifyRequest);
    }
  };
RestexampleApi.prototype.User.apiObjectType = "resourceDir";
RestexampleApi.prototype.User.list =
  function (success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
RestexampleApi.prototype.User.create =
  function (json, success, error, params, callOpts)
  {
    return RestexampleApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };  return new RestexampleApi (url, secureUrl, modifyRequest);
}

var jqFun;
if (isNodeJs)
{
  // Export as Node module.
  module.exports = RestexampleApi;
}
else
{
  if (isCommonJs) {
    // Export as CommonJs
    module.exports = RestexampleApi;
  } else if (typeof define === "function" && define.amd) {
    // Export as AMD.
    define("RestexampleApi", [], function () { return RestexampleApi; });
  } else {
    // Export as global.
    window.RestexampleApi = RestexampleApi;
  }
}

})(this);