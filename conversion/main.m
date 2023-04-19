I = imread("../images/homme_gris.jpeg");
grayI = rgb2gray(I);
imwrite(grayI, "../images/femme3_gris2.jpeg");
imshow(grayI);
waitfor(gcf);
