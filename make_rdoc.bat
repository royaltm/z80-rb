rdoc --encoding=UTF-8 --title=ruby-Z80 --main=README.rdoc README.rdoc z80.rb z80/*.rb zxlib/*.rb utils/*.rb
rem cd doc
rem mkdir -p examples
rem cp -v ../examples/*.{jpg,png} examples/
rem git init
rem git remote add -t 'gh-pages' -f origin git@github.com:royaltm/z80-rb.git
rem git checkout 'gh-pages'
