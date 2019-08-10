%{
Created by Steven Jennings on 5 March 2018.

This is Part 4 of the assignment.

DO NOT RUN THIS FILE.
%}

% Clean up workspace
% close all
% clear all
% clc

% Declare resulting images
im_gauss_restored = [];
im_uniform_restored = [];
im_snp_restored = [];

%{
Restore Gaussian-corrupted image
%}

% Apply median filter
temp = imsmooth(im_gauss, "median", 11);

% Apply unsharp filter
temp = imfilter(temp, fspecial("unsharp"));

% Apply another median filter
temp = imsmooth(temp, "median", 21);

% Apply Laplacian enhancement
im_gauss_restored = temp .+ imfilter(temp, fspecial("laplacian"));

% Show Gaussian restoration results
figure;
imshow(im_gauss_restored);
title("Restored Gaussian Noise");

% Show Gaussian restored histogram
figure;
plot(imhist(im_gauss_restored));
title("Restored Gaussian Histogram");

%{
Restore Uniform-corrupted image
%}

% Apply median filter
im_uniform_restored = imsmooth(im_uniform, "median", 11);

% Show Uniform restoration results
figure;
imshow(im_uniform_restored);
title("Restored Uniform Noise");

% Show Uniform restored histogram
figure;
plot(imhist(im_uniform_restored));
title("Restored Uniform Histogram");

%{
Restore Salt & Pepper-corrupted image
%}

% Apply median filter
im_snp_restored = imsmooth(im_snp, "median", 11);

% Show Salt & Pepper restoration results
figure;
imshow(im_snp_restored);
title("Restored Salt & Pepper Noise");

% Show Salt & Pepper restored histogram
figure;
plot(imhist(im_snp_restored));
title("Restored Salt & Pepper Histogram");

% Write images to persistent files
imwrite(im_gauss_restored, "./Restored Images/im_gauss_restored.jpg");
imwrite(im_uniform_restored, "./Restored Images/im_uniform_restored.jpg");
imwrite(im_snp_restored, "./Restored Images/im_snp_restored.jpg");

% Save all figures
saveas(1, "./Figures/Figure 1.png", "png");
saveas(2, "./Figures/Figure 2.png", "png");
saveas(3, "./Figures/Figure 3.png", "png");
saveas(4, "./Figures/Figure 4.png", "png");
saveas(5, "./Figures/Figure 5.png", "png");
saveas(6, "./Figures/Figure 6.png", "png");
saveas(7, "./Figures/Figure 7.png", "png");
saveas(8, "./Figures/Figure 8.png", "png");
saveas(9, "./Figures/Figure 9.png", "png");