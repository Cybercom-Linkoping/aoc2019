def count_digits(layer):
    digits = [0]*10
    for pixel in range(len(layer)):
        digits[int(layer[pixel])] += 1
    return digits

def read_layers_and_count_digits(imgdata, pixels):
    position = 0
    digit_counts = []
    layers = []
    while position < len(imgdata):
        layer = imgdata[position:position+pixels]
        layers.append(layer)
        digit_counts.append(count_digits(layer))
        position += pixels
    return layers, digit_counts

def find_answer_to_part1(digit_counts, pixels):
    fewestzeros = pixels
    for digit_count in digit_counts:
        if digit_count[0] < fewestzeros:
            fewestzeros = digit_count[0]
            resultforlayerwithfewestzeros = digit_count
    print resultforlayerwithfewestzeros[1]*resultforlayerwithfewestzeros[2]

def flatten_layers(layers, pixels):
    transparent = [True]*pixels
    flattenedimg = [2]*pixels
    for layer in layers:
        for pixel in range(pixels):
            if (transparent[pixel] == True) and (layer[pixel] != "2"):
                transparent[pixel] = False
                flattenedimg[pixel] = layer[pixel]
    return flattenedimg

def print_flattened_image(img, height, width):
    for row in range(height):
        for column in range(width):
            if img[width*row+column] == "0":
                print " ",
            else:
                print "#",  
        print


def main():
    width = 25
    height = 6
    pixels = width*height
    with open("input.txt", "rt") as imgfile:
        imgdata = imgfile.readline().strip()

    layers, digit_counts = read_layers_and_count_digits(imgdata, pixels)
    find_answer_to_part1(digit_counts, pixels)

    flattenedimg = flatten_layers(layers, pixels)
    print_flattened_image(flattenedimg, height, width)
    
main()
