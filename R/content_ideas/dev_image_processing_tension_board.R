install.packages("magick")
install.packages("imager")
install.packages("jpeg")
library(jpeg)
library(magick)
library(imager)
library(tidyverse)

jpeg::readJPEG()
magick::image_read()
# Load the image
img_path <- "C:/Users/GauntM/Desktop/thumbnail.jpg"  # Replace with your image file path
img <- image_read(img_path)
img %>% image_scale("200x")  %>% image_quantize(128)
img %>%  count()
plot(img)
str(img)
img[,,1] %>% as.numeric() %>% .[1:10]
img[,,2] %>% as.numeric() %>% .[1:10]
img[1, 1, 1]
image_10 = image_quantize(img, max = 10, colorspace = 'rgb') %>%
  image_raster()
1080/2
img %>%
  image_crop("740x250+180+240") %>%
  image_ocr()
plot()

temp_plot = image_10 %>%
  # image_resize()
  # magick::image_ocr()
  #deg_pixel range
  filter(between(y, 0, 600) #&
         # between(x, 150, 250)
  ) %>%
  filter(col != "#040404ff") %>%
  # filter(#y < 1000 &
  # y < 500) %>%
  ggplot() +
  geom_tile(aes(x, y, fill = as.factor(col)))


plotly::ggplotly(temp_plot)



dim(image_10)

small  = image_10 %>%
  count(col) %>%
  arrange(n) %>%
  head(2)

image_10 %>%
  merge(small) %>%
  # filter(col != "#040404ff") %>%
  # filter(col %in% c("#a13d27ff"))
  # sample_frac(.5) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = as.factor(col))) +
  scale_y_reverse() +
  coord_equal()

image_10 %>%
  merge(small) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = as.factor(col))) +
  scale_y_reverse() +
  coord_equal()


df = image_10 %>%
  merge(small) %>%
  filter(#y < 1000 &
    x < 150)

db = df %>%
  select(x, y) %>%
  dbscan::dbscan(eps = 50, minPts = 40)



tt = df %>%
  mutate(cluster = as.factor(db$cluster))

df_point = tt %>%
  group_by(cluster) %>%
  summarise(x = mean(x), y = mean(y), cnt = n()) %>%
  arrange(cnt) %>%
  filter(cnt > 1000)


tt %>%
  filter(cluster %in% df_point$cluster) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = cluster)) +
  geom_point(data = df_point, aes(x, y, color = cluster)) +
  scale_y_reverse() +
  coord_equal()




plotly::plot_ly(as.data.frame(img))
as.data.frame(img)
class(img)
boats = img

im <- load.image(img_path)

list(Red  = im[,,1],
     Green= im[,,2],
     Blue = im[,,3])

# Convert to an 'imager' image for analysis
img_cimg <- magick2cimg(img)
str(img_cimg)
# Function to find coordinates of a specific color
# Adjust `r_min`, `g_min`, `b_min` and their max counterparts to tune the color filtering
find_color_coords <- function(img, r_min, r_max, g_min, g_max, b_min, b_max) {
  # Threshold the image based on color
  img_color <- img %>%
    as.data.frame() %>%
    dplyr::filter(r >= r_min, r <= r_max,
                  g >= g_min, g <= g_max,
                  b >= b_min, b <= b_max)

  # Calculate the average x and y coordinates for each color (center of the circle)
  if (nrow(img_color) > 0) {
    x_coord <- mean(img_color$x)
    y_coord <- mean(img_color$y)
    return(data.frame(x = x_coord, y = y_coord))
  } else {
    return(NULL)
  }
}


img_cimg %>% dim()

img_cimg[,,1,1]
# Define color ranges for red, blue, green, magenta (example values, you may need to adjust)
red_coords <- find_color_coords(img_cimg, r_min = 0.8, r_max = 1, g_min = 0, g_max = 0.2, b_min = 0, b_max = 0.2)
blue_coords <- find_color_coords(img_cimg, r_min = 0, r_max = 0.2, g_min = 0, g_max = 0.2, b_min = 0.8, b_max = 1)
green_coords <- find_color_coords(img_cimg, r_min = 0, r_max = 0.2, g_min = 0.8, g_max = 1, b_min = 0, b_max = 0.2)
magenta_coords <- find_color_coords(img_cimg, r_min = 0.8, r_max = 1, g_min = 0, g_max = 0.2, b_min = 0.8, b_max = 1)

# Combine all coordinates into a data frame
all_coords <- dplyr::bind_rows(
  dplyr::mutate(red_coords, color = "Red"),
  dplyr::mutate(blue_coords, color = "Blue"),
  dplyr::mutate(green_coords, color = "Green"),
  dplyr::mutate(magenta_coords, color = "Magenta")
)

# Print the coordinates
print(all_coords)
