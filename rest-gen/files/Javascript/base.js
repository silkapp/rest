$apinamespace$ =
  function (url, secureUrl)
  {
    var postfix = '/v' + this.version + '/';
    var sUrl = url + postfix;
    $apinamespace$.setContext(this, sUrl);
    this.contextUrl = sUrl;
    this.secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;
  };
$apinamespace$.addObject =
  function (obj1, obj2)
  {
    for (var fld in obj2)
    {
      obj1[fld] = obj2[fld];
    }
  };
$apinamespace$.ajaxCall =
  function (method, url, params, success, error, contentType, dataType, data, callOpts)
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
    $apinamespace$.addObject(callData, callOpts);
    return $dollar$.ajax(callData);
  };
$apinamespace$.setContext =
  function (obj, url)
  {
    for (var fld in obj)
    {
      if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
      {
        var newUrl = url + fld.replace(/([a-z0-9])([A-Z])/g, '$dollar$1-$dollar$2').toLowerCase() + '/'
        $apinamespace$.setContext(obj[fld], newUrl);
        obj[fld].contextUrl = newUrl;
      }
    }
  };
$apinamespace$.makeSilkConstructor =
  function ()
  {
    return function constrWrapper (url)
           {
             if (this instanceof constrWrapper)
             {
               $apinamespace$.setContext(this, url);
               this.contextUrl = url;
             }
             else
             {
               return constrWrapper.access(url);
             }
           };
  };
