I = imread("../images/femme3.jpeg");
grayI = rgb2gray(I);
imwrite(grayI, "../images/femme3_gris.jpeg");
imshow(grayI);
waitfor(gcf);
