%{
Created by Steven Jennings on 11 February 2018.
%}

% Clean up workspace
clc;
clear all;
close all;

% Fetch image data
im = imread('./iris(1).bmp');
im = double(im);

% Display original image in first quadrant
figure,
subplot(2,2,1);
imagesc(im);
colormap(gray(256));

% Fetch image dimensions
[rows, cols] = size(im);

% Edge detection by Sobel Filter
SH = [-1 -2 -1
       0  0  0
       1  2  1];
SV = [-1  0  1
      -2  0  2
      -1  0  1];
temp0 = imfilter(im, SH, "same");
temp1 = imfilter(im, SV, "same");
im_edge = sqrt((temp0.^2 + temp1.^2) / 2);

% Selecting a radius that would best fit the pupil/circle
r = 37;

tfill = zeros(r * 2 + 1);
tcircle(1, r + 1) = 1;
tcircle(r * 2 + 1, r + 1) = 1;
mn_ang = 1 / r; % Set minimum angle (value) at which to increment through 90 degrees (pi/2)
for i = 0:mn_ang:pi / 2
    y = round(r * cos(i));
    x = round(r * sin(i));
    tcircle(r + 1 + x, r + 1 + y) = 1;
    tcircle(r + 1 + x, r + 1 - y) = 1;
    tcircle(r + 1 - x, r + 1 + y) = 1;
    tcircle(r + 1 - x, r + 1 - y) = 1;
    if y > 5 & r + 1 - x > 5 & r + 1 + x < r * 2 - 5
        tfill(r + 1 - x, r + 1 - y + 5:r + 1 + y - 5) = 1;
        tfill(r + 1 + x, r + 1 - y + 5:r + 1 + y - 5) = 1;
    end
end

% Display filtered image after Sobel filtering
im3 = imfilter(im_edge, tcircle, "same");
subplot(2, 2, 2);
imagesc(im3);
colormap(gray(256));

% Prepare to detect center of pupil
im4 = uint8(zeros([rows, cols]));

% Use thresholding to determine pupil center
max_val = max(im3(:));
for i = 1:rows
  for j = 1:cols
    if im3(i, j) > 0.99 * max_val % 99% of max value
      im4(i, j) = 255;
      point_y = i; % Y coordinate of pupil center
      point_x = j; % X coordinate of pupil center
    else
      im4(i, j) = 0;
    end
  end
end

% Display center point of Pupil
subplot(2,2,3);
imshow(mat2gray(im4));
colormap(gray(256));

% Prepare result
result = im_edge;

% Remove noise based on distance from pupil center
for i = 1:rows
  for j = 1:cols
    if sqrt((i - point_y)^2 + (j - point_x)^2) > (r + 3) % Check for r + 3 because we want to include all of the actual circle edge
      result(i, j) = 0;
    end
  end
end

% Display detected boundary
subplot(2,2,4);
imagesc(result);
colormap(gray(256));

% Write output images
imwrite(mat2gray(im), "output_0.jpg");
imwrite(mat2gray(im3), "output_1.jpg");
imwrite(mat2gray(im4), "output_2.jpg");
imwrite(mat2gray(result), "output_3.jpg");