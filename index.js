var fs = require('fs');
var slash = require('slash');
var recursive = require('recursive-readdir');

var elmPackageJson = JSON.parse(fs.readFileSync('./elm-package.json', 'utf8'));
var srcDir = process.cwd() + '/' + elmPackageJson['source-directories'][0];// TODO normalize, multi dir


getDeps((e, data) => {
  if(e) {
    throw e;
  }

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
      files = files.filter((file) => {
        return file.endsWith('.elm');
      });

      var myModules = {};
      files.forEach(function(file) {
        var relPathSlash = slash(file).split(slash(srcDir + '/'))[1];
        myModules[relPathSlash.replace(/\//g, '.').replace(/\.elm/, '')] = true
      });

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
        var altModName = slash(file).split('/').pop().split('.')[0];
        return [ moduleName || altModName, imports ];
      });
      cb(null, {
        deps: deps
      });
    }
  });
}
