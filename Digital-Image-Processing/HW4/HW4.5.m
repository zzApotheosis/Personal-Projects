%{
Created by Steven Jennings on 7 March 2018.

This is an optional Part 5 of the assignment I tried out.

I tried generating a number of noisy images and used pixel averaging to
try to restore the original image, like the way Google's HDR+ processing
restores/enhances images in their latest smartphones.

For the math nerds: As numIms approaches infinity, result approaches the
original image.
%}

% Clean up workspace
clear all
close all
clc
delete("./Optional/Noisy Images/*.jpg");

% Create variables
x = 256;
y = 256;
center = [x / 2, y / 2];
radius = 80;
numIms = 10;

% Create "perfect" image
im = [];
for i = 1:x
  for j = 1:y
    if sqrt((i - center(1)) ^ 2 + (j - center(2)) ^ 2) < radius
      im(i, j) = 0.4;
    else
      im(i ,j) = 0.7;
    end
  end
end

imwrite(im, "./Optional/generated_image.jpg");
figure;
imshow(im);
title("\"Perfect\" Image");
figure;
imhist(im);
title("\"Perfect\" Image Histogram");

% Create noisy images (3D matrix)
ims = zeros([x, y, numIms]);
for i = 1:numIms
  ims(:, :, i) = imnoise(im, "gaussian", 0, 0.01);
  %figure;
  %imshow(ims(:, :, i));
  imwrite(ims(:, :, i), strcat("./Optional/Noisy Images/noise_", num2str(i), ".jpg"));
end

% Image restoration (Average each image value)
result = zeros([x, y]);
for i = 1:numIms
  result(:, :) = result(:, :) .+ ims(:, :, i) / numIms;
end

imwrite(result, "./Optional/HDR+_result.jpg");

% Show restoration result
figure;
imshow(result);
title("HDR+ Result");
figure;
imhist(result);
title("HDR+ Result Histogram");