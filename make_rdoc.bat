rdoc --encoding=UTF-8 --title=ruby-Z80 --main=README.rdoc README.rdoc LICENSE.md lib/z80.rb lib/z80/*.rb lib/z80/utils/*.rb lib/zxlib/*.rb lib/zxlib/*/*.rb lib/zxutils/*.rb lib/zxutils/*/*.rb
rem cd doc
rem mkdir -p examples
rem cp -v ../examples/*.{jpg,png} examples/
rem git init
rem git remote add -t 'gh-pages' -f origin git@github.com:royaltm/z80-rb.git
rem git checkout 'gh-pages'
