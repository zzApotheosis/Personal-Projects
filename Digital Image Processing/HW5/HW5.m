%{
Created by Steven Jennings on 8 March 2018.
%}

% Clean up workspace
clear all
close all
clc

% Fetch input image
im = imread("./DJI_0017.JPG");
im = im(1000:1800, 2300:2800, :); % Crop for just my face
im_size = size(im);

% Write original cropped image
imwrite(im, "out_0.jpg");

% Display original cropped image
figure;
imshow(im);

% My skin detection (With HSV Colorspace)
im = rgb2hsv(im);

% Display data
figure;
plot(imhist(im(:,:,1)));
title("Hue");

figure;
plot(imhist(im(:,:,2)));
title("Saturation");

figure;
plot(imhist(im(:,:,3)));
title("Value");

% Create binary images
binH = 0 / 255 <= im(:, :, 1) & im(:, :, 1) <= 255 / 255;
binS = 0 / 255 <= im(:, :, 2) & im(:, :, 2) <= 100 / 255;
binV = 100 / 255 <= im(:, :, 3) & im(:, :, 3) <= 227 / 255;

% Display binary images
figure;
imshow(binH);
title("binH");

figure;
imshow(binS);
title("binS");

figure;
imshow(binV);
title("binV");

bin_im = binH & binS & binV;
figure;
imshow(bin_im);
title("bin_im");

% Fill holes (Mainly just the eyes)
bin_im = imfill(bin_im, "holes");
figure;
imshow(bin_im);

% Face detected
result = hsv2rgb(im) .* bin_im;

% Write result
imwrite(result, "out_1.jpg");

%{

close all
sleep(0.2)

%}

% Dr. Jiang's skin detection
im = hsv2rgb(im);
ims1 = (im(:,:,1) > 95 / 255) & (im(:,:,2) > 40 / 255) & (im(:,:,3) > 20 / 255);
ims2 = (im(:,:,1) - im(:,:,2) > 15 / 255) | (im(:,:,1) - im(:,:,3) > 15 / 255);
ims3 = (im(:,:,1) - im(:,:,2) > 15 / 255) & (im(:,:,1) > im(:,:,3));
ims = ims1 & ims2 & ims3;
figure,imshow(ims1);
figure,imshow(ims2);
figure,imshow(ims3);
figure,imshow(ims);
title('skin detected before correction');

% Fill holes (Mainly just the eyes)
ims = imfill(ims, "holes");
figure;
imshow(ims);
title("Skin detection with holes filled");

% Image Closing to remove 
ims = imclose(ims, strel("disk", 30, 0));
figure;
imshow(ims);
title("Skin detection with closing");

% Image Opening to remove small false-positives
ims = imopen(ims, strel("disk", 30, 0));
figure;
imshow(ims);
title("Skin detect with closing and opening");

% Final Result
im = im .* ims;
figure;
imshow(im);
title("Face detected");

% Write Final Result
imwrite(im, "out_2.jpg");