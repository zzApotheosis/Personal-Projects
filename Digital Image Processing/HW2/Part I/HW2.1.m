%{
Created by Steven Jennings on 07 February 2018.
%}

% Clean up workspace
clc;
clear;
close all;

% Fetch raw image data
in0 = imread("./input.jpg");
in1 = rgb2gray(in0); % Convert to grayscale

% Fetch dimensions
[rows1, cols1] = size(in1);

% Averaging filter (9x9 kernel)
f0 = fspecial("average", 9);
out1 = imfilter(in1, f0, "replicate");

% Sobel filter
FH1 = 0.005.*[-1 -2 -1
               0  0  0
               1  2  1];
FV1 = 0.005.*[-1 0 1
              -2 0 2
              -1 0 1];
temp0 = imfilter(double(in1), FH1, "same");
temp1 = imfilter(double(in1), FV1, "same");
out2 = sqrt((temp0.^2 + temp1.^2) / 2); % Apply Pythagoras' Theorem to each value in both temp matrices

% Laplacian filter
FL = [0  1  0;
      1 -4  1;
      0  1  0];
out3 = imfilter(in1, FL, "same");

% Median filter (17x17 kernel)
out4 = imsmooth(in1, "median", 17);

% Write outputs
imwrite( in1, "output_0.jpg"); % Grayscale original
imwrite(out1, "output_1.jpg"); % Averaging
imwrite(out2, "output_2.jpg"); % Sobel
imwrite(out3, "output_3.jpg"); % Laplacian
imwrite(out4, "output_4.jpg"); % Median

% Display outputs
figure;

% Averaging
subplot(2,2,1);
imshow(out1);

% Sobel
subplot(2,2,2);
imshow(out2);

% Laplacian
subplot(2,2,3);
imshow(out3);

% Median
subplot(2,2,4);
imshow(out4);