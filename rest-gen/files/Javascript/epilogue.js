  return new $apinamespace$ (url, secureUrl, modifyRequest);
}

var jqFun;
if (isNodeJs)
{
  // Export as Node module.
  module.exports = $apinamespace$;
}
else
{
  if (isCommonJs) {
    // Export as CommonJs
    module.exports = $apinamespace$;
  } else if (typeof define === "function" && define.amd) {
    // Export as AMD.
    define("$apinamespace$", [], function () { return $apinamespace$; });
  } else {
    // Export as global.
    window.$apinamespace$ = $apinamespace$;
  }
}
