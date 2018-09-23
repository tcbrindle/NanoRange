
./: lib{nanorange} test/ doc{README.md LICENSE_1_0.txt} manifest

lib{nanorange}: hxx{include/***}
cxx.poptions += "-I$out_base/include" "-I$src_base/include"
lib{nanorange}: cxx.export.poptions = "-I$out_base/include" "-I$src_base/include"
lib{nanorange}: install.subdirs = true

# If building with msvc, require the /permissive- switch
if ($cxx.export.class == 'msvc')
{
    lib{nanorange}: cxx.export.coptions = "/permissive-"
}

test/: install = false
tools/: install = false
