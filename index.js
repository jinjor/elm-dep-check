var fs = require('fs');
var slash = require('slash');
var recursive = require('recursive-readdir');
var path = require('path');

function report(options) {
  options = Object.assign({}, {
    output: './dep-check.html'
  }, options || {})
  analyze(getProjectInfo().srcDirs).then((data) => {
    fs.writeFileSync(options.output, data);
  }).catch((e) => {
    console.log(e);
    throw e;
  });
}

function getProjectInfo() {
  if(!fs.existsSync('./elm-package.json')) {
    throw 'elm-package.json does not exist in current directory';
  }
  var elmPackageJson = JSON.parse(fs.readFileSync('./elm-package.json', 'utf8'));

  var sourceDirectories = elmPackageJson['source-directories'];
  if(!sourceDirectories) {
    throw '"source-directories" is not defined in elm-package.json';
  }

  var cwd = process.cwd();
  var srcDirs = sourceDirectories.map((dir) => {
    return path.normalize(cwd + '/' + dir);
  });

  return {
    srcDirs: srcDirs
  };

}

function analyze(srcDirs) {
  return getDeps(srcDirs).then((allDeps) => {
    var data = {
      deps: allDeps
    };
    var depCheckSource = fs.readFileSync(__dirname + '/dest/dep-check.js', 'utf8');
    var template = fs.readFileSync(__dirname + '/dest/template.html', 'utf8');
    var output = template
      .replace(/{{src}}/, depCheckSource)
      .replace(/{{data}}/, JSON.stringify(data));
    return Promise.resolve(output);
  });
}

function getDeps(srcDirs) {
  return Promise.all(srcDirs.map(getDepsEach)).then((depsList) => {
    var allDeps = Array.prototype.concat.apply([], depsList);
    return Promise.resolve(allDeps);
  });
}

function getDepsEach(srcDir) {
  return new Promise((resolve, reject) => {
    recursive(srcDir, function (e, files) {// TODO ignore others
      if(e) {
        reject(e);
      } else {
        files = files.filter((file) => {
          return file.endsWith('.elm');
        });
        var deps = files.map(function(fileName) {
          var code = fs.readFileSync(fileName, 'utf8');
          var moduleName = getModuleName(srcDir, fileName, code);
          var imports = getImports(code);
          return [ moduleName, imports ];
        });
        resolve(deps);
      }
    });
  });
}

function getModuleName(srcDir, fileName, code) {
  var moduleName = code.split('\n').filter((line) => {
    return line.startsWith('module ');
  }).map((line) => {
    return line.split('module')[1].split(' exposing')[0].trim();
  })[0];
  var altModName = slash(fileName).split(slash(srcDir))[1].split('/').pop().split('.')[0];//FIXME x/y/Main.elm => x.y.Main
  return moduleName || altModName;
}

function getImports(code) {
  return code.split('\n').filter((line) => {
    return line.startsWith('import ');
  }).map((line) => {
    var name = line.split('import')[1].split(' exposing')[0].split(' as ')[0].trim();
    return name;
  });
}

module.exports = {
  report: report
};
