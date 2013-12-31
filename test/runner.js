var p = require('webpage').create();
var fs = require('fs');
var sys = require('system');

p.onConsoleMessage = function(x) {
  var line = x.toString();
  if (line !== "[NEWLINE]") {
    console.log(line.replace(/\[NEWLINE\]/g, "\n"));
  }
};

p.open("public/tests.html", function(status) {
  if (status != "success") {
    console.log("unable to open tests.html");
    sys.exit(1);
  } else {
    var result = p.evaluate(function() {
      cemerick.cljs.test.set_print_fn_BANG_(function(x) {
        console.log(x.replace(/\n/g, "[NEWLINE]")); // since console.log *itself* adds a newline
      });

      var results = cemerick.cljs.test.run_all_tests();
      console.log(results);
      return cemerick.cljs.test.successful_QMARK_(results);
    });

    if (result) {
      console.log("All tests passed");
      phantom.exit(0);
    } else {
      console.log("Some tests failed");
      phantom.exit(2);
    }
  }
});
