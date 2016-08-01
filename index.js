var fs = require('fs');
var slash = require('slash');
var recursive = require('recursive-readdir');

var elmPackageJson = JSON.parse(fs.readFileSync('./elm-package.json', 'utf8'));
var srcDir = process.cwd() + '/' + elmPackageJson['source-directories'][0];// TODO normalize, multi dir


getDeps((e, data) => {
  if(e) {
    throw e;
  }
  // console.log(data.deps);

  var depCheckSource = fs.readFileSync(__dirname + '/dest/dep-check.js', 'utf8');
  var template = fs.readFileSync(__dirname + '/dest/template.html', 'utf8');
  var output = template.replace(/{{src}}/, depCheckSource).replace(/{{data}}/, JSON.stringify(data));

  fs.writeFileSync('./dep-check.html', output);
});

function getDeps(cb) {
  recursive(srcDir, function (e, files) {// TODO ignore others
    if(e) {
      cb(e)
    } else {
      var myModules = {};
      files.forEach(function(file) {
        var relPathSlash = slash(file).split(slash(srcDir + '/'))[1];
        myModules[relPathSlash.replace(/\//, '.').replace(/\.elm/, '')] = true
      });
      // console.log(myModules);

      var deps = files.map(function(file) {
        var code = fs.readFileSync(file, 'utf8');
        var moduleName = code.split('\n').filter((line) => {
          return line.startsWith('module');
        }).map((line) => {
          return line.split('module')[1].split('exposing')[0].trim();
        })[0];
        var imports = code.split('\n').filter((line) => {
          return line.startsWith('import');
        }).map((line) => {
          var name = line.split('import')[1].split('exposing')[0].split('as')[0].trim();
          return [name, myModules[name] || false];
        });
        return [ moduleName || 'Main', imports ];
      });
      cb(null, {
        deps: deps
      });
    }
  });
}
