library(showtext)
library(hexSticker)
font_add_google("Righteous", "scribblr_font")
## Automatically use showtext to render text for future devices
showtext_auto()

print(
	sticker(package = "scribblr"
			,p_family = "scribblr_font"
			,p_color = "#000000"
			,p_size = 36,
			,p_y = 1.35
			,h_fill = "white"
			,h_color = "black"
			,subplot = "img/feather.png"
			,s_x = 1, s_y = .65, s_width = 0.55, s_height = 0.55

			,filename = "img/logo.png"
	))

usethis::use_logo("img/logo.png")

