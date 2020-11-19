# frozen_string_literal: true

SCREENS = %w[
  car.scr
  car_hicolor.scr
  car_hicolor_dither.scr
  car_hires_0.scr
  car_hires_0_dither.scr
  car_hires_1.scr
  car_hires_1_dither.scr
  car_hires+_dither.scr
  car_interlace.scr
  car_64_dither.scr
  car_ham256.scr
  car_ham256_dither.scr
  car_64_hicolor.scr
  car_64_hicolor_dither.scr
  car_ham256hicolor.scr
  car_ham256hicolor_dither.scr
  creek_ham256_hicolor.scr
  forest_ham256_hicolor.scr
  lake_ham256_hicolor.scr
  organs_ham256_hicolor.scr
  wfall_64.scr
  wfall_64_hicolor.scr
  wfall_ham256_hicolor.scr
  test_ham256.scr
]

namespace :example do
  desc "Create an example gallery"
  task :gallery do 
    paths = SCREENS.map {|s| File.join("examples","screens",s) }
    sh "zxgallery -o examples/example_gallery.tap #{paths.join(' ')}"
  end
end
