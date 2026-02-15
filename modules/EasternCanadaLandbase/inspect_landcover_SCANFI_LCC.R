library(terra)

# مسیر فایل لندکاور (کل کانادا، 30m)
lc_path <- "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"

# خواندن به‌صورت SpatRaster (lazy load)
lc <- terra::rast(lc_path)
lc

# اطلاعات پایه
terra::crs(lc)
terra::ext(lc)
terra::res(lc)

# ---- بررسی کلاس‌ها (ایمن برای raster بزرگ) ----

# فراوانی کلاس‌ها (بدون بارگذاری کل raster در RAM)
freq_lc <- terra::freq(lc)

# نمایش جدول فراوانی
freq_lc

# مرتب‌سازی بر اساس بیشترین تعداد سلول
freq_lc_sorted <- freq_lc[order(-freq_lc$count), ]
freq_lc_sorted

# کدهای کلاس موجود
class_codes <- sort(freq_lc$value)
class_codes

# تعداد کلاس‌ها
length(class_codes)

# ---- خلاصه‌ی کلی ----
summary(lc)

# ---- نمایش خیلی خام (اختیاری) ----
plot(lc, main = "SCANFI Land Cover – raw classes")

