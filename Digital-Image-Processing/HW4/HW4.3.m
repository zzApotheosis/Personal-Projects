%{
Created by Steven Jennings on 5 March 2018.

This is Part 3 of the assignment.

DO NOT RUN THIS FILE.
%}

% Clean up workspace
% close all
% clear all
% clc

% Get actual noise matrices
noise_gauss_raw = reference .- noise_gauss;
noise_uniform_raw = reference .- noise_uniform;
noise_snp_raw = reference .- noise_snp;

% Add each noise type to original image
im_gauss = im .+ noise_gauss_raw;
im_uniform = im .+ noise_uniform_raw;
im_snp = im .+ noise_snp_raw;

% Gaussian Noise
figure;
subplot(2, 2, 1);
imshow(im);
title("Original Image");
subplot(2, 2, 2);
imshow(im_gauss);
title("Gaussian Noise Added");
subplot(2, 2, 3);
plot(imhist(im));
title("Original Histogram");
subplot(2, 2, 4);
plot(imhist(im_gauss));
title("Gaussian Noise Histogram");

% Uniform Noise
figure;
subplot(2, 2, 1);
imshow(im);
title("Original Image");
subplot(2, 2, 2);
imshow(im_uniform);
title("Uniform Noise Added");
subplot(2, 2, 3);
plot(imhist(im));
title("Original Histogram");
subplot(2, 2, 4);
plot(imhist(im_uniform));
title("Uniform Noise Histogram");

% Salt & Pepper Noise
figure;
subplot(2, 2, 1);
imshow(im);
title("Original Image");
subplot(2, 2, 2);
imshow(im_snp);
title("Salt & Pepper Noise Added");
subplot(2, 2, 3);
plot(imhist(im));
title("Original Histogram");
subplot(2, 2, 4);
plot(imhist(im_snp));
title("Salt & Pepper Noise Histogram");

% Write images to persistent files
imwrite(im_gauss, "./Images with Noise/im_gauss_noise.jpg");
imwrite(im_uniform, "./Images with Noise/im_uniform_noise.jpg");
imwrite(im_snp, "./Images with Noise/im_snp_noise.jpg");