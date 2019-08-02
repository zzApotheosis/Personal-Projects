%{
Created by Steven Jennings on 11 April 2018.

Make sure to drop all desired image files in the ./in/ folder and run the Python
script before you run this script. The Python script is what actually generates
the saliency maps, which we use here. Make sure Python version 2 is used, not
version 3.
%}

% Clean up workspace
clear all;
close all;
clc;
delete("./out/*");
addpath(".");

% Set up for the problem
input_dir = "./in/";
saliency_dir = "./sal/";
output_dir = "./out/";
imlist = dir(strcat(input_dir, "*.jpg")); % Must be .jpg (JPEG) images
sallist = dir(strcat(saliency_dir, "*.jpg")); % Directory with saliency maps
r = 480; % Rows
c = 640; % Columns
num_ims = length(imlist); % Number of .jpg (JPEG) images in the input folder
                          % This is also the number of saliency maps available
f = fopen(strcat(output_dir, "data.txt"), "w"); % We want to write data to a file
tolerance = 0.01; % Tolerance for custom metric. Negative value means completely intolerant

% Process each image
for i = [1:num_ims]
  % Fetch input data for image i
  temp_im = imread(strcat(input_dir, imlist(i).name)); % Input image
  if length(size(temp_im)) > 2 % If the input image is color
    temp_im = rgb2gray(temp_im); % Convert to grayscale
  end
  temp_im = resize(temp_im, [r, c]);
  uniform_noise = unifrnd(-0.1, 0.1, [r, c]); % Input image with artificial uniform noise
  temp_sal = imread(strcat(saliency_dir, sallist(i).name)); % Corresponding saliency map
  
  % Normalize data, assuming min 0, and max 255
  mn = 0;
  mx = 255;
  im = (double(temp_im) .- mn) ./ (mx - mn); % Normalize input image
  n = im .+ uniform_noise; % Create noise image
  sal = (double(temp_sal) .- mn) ./ (mx - mn); % Normalize saliency map
  
  % Fix max/min values in noisy image, because the effect of noise usually puts
  % some values over 1 and under 0. This will heavily decrease performance, with
  % not much benefit.
  %{
  for j = [1:r]
    for k = [1:c]
      if n(j, k) > 1.0
        n(j, k) = 1.0;
      elseif n(j, k) < 0.0
        n(j, k) = 0.0;
      end
    end
  end
  %}
  
  % Calculate Mean Squared Error (MSE) with artificially created noisy image
  
  %{
  Note: The following nested for-loop is the raw math, and it is horribly optimized,
  but the built-in function works just fine. So I use immse() instead.
  %}
  
  %{
  mse = 0.0;
  for j = [1:r]
    for k = [1:c]
      mse = mse + ((n(j, k) - im(j, k)) ^ 2) / (r * c); % MSE; test image minus reference image
    end
  end
  %}
  
  normal_mse = immse(n, im); % Mean Squared Error
  
  % Calculate Peak Signal to Noise Ratio (PSNR) from MSE
  normal_psnr = 20 * log10(max(im(:))) - 10 * log10(normal_mse);
  
  % Calculate Structural Similarity (SSIM)
  normal_ssim = ssim(n, im);
  
  % Calculate Custom Metric
  normal_custom_metric = cmet(n, im, tolerance);
  
  %{
  At this point, the MSE and PSNR have been calculated with the basic formulae.
  
  Now we must calculate the WEIGHTED MSE and PSNR with the saliency map.
  %}
  
  weighted_im = im .* sal; % WEIGHTED image
  weighted_n = n .* sal; % WEIGHTED noisy image
  
  % Calculate MSE for weighted images
  weighted_mse = immse(weighted_n, weighted_im);
  
  % Calculate PSNR for weighted images
  weighted_psnr = 20 * log10(max(weighted_im(:))) - 10 * log10(weighted_mse);
  
  % Calculate SSIM for weighted images
  weighted_ssim = ssim(weighted_n, weighted_im);
  
  % Calculate Custom Metric (My own metric)
  weighted_custom_metric = cmet(weighted_n, weighted_im, tolerance);
  
  % Write data to file
  fwrite(f, sprintf("Image #%d\n", i - 1));
  fwrite(f, sprintf("Unweighted MSE: %f\n", normal_mse));
  fwrite(f, sprintf("Unweighted PSNR: %f\n", normal_psnr));
  fwrite(f, sprintf("Unweighted SSIM: %f\n", normal_ssim));
  fwrite(f, sprintf("Unweighted Custom Metric: %f\n", normal_custom_metric));
  fwrite(f, sprintf("Weighted MSE: %f\n", weighted_mse));
  fwrite(f, sprintf("Weighted PSNR: %f\n", weighted_psnr));
  fwrite(f, sprintf("Weighted SSIM: %f\n", weighted_ssim));
  fwrite(f, sprintf("Weighted Custom Metric: %f\n\n", weighted_custom_metric));
  
  % Write images to files
  imwrite(im, strcat(output_dir, sprintf("%d_grayscale_original.jpg", i - 1)));
  imwrite(n, strcat(output_dir, sprintf("%d_noisy.jpg", i - 1)));
end

% Clear temporary variables
clear mn;
clear mx;
clear temp_im;
clear uniform_noise;
clear temp_sal;
clear im;
clear n;
clear sal;
clear weighted_im;
clear weighted_n;
clear i;

% Flush and close file writer
fflush(f);
fclose(f);
clear f;
clear ans;

%{
So at this point, it is clear that using weighted quality metrics with saliency
gives much more accurate results. The "normal" quality metrics always showed
"worse" quality results than the weighted quality metrics, which means that the
weighted quality metrics focused on the important parts of the images instead of
the non-important parts, yielding "better" quality metrics.

In reality, people typically don't care very much about the quality of a
background in any given image. This is why saliency seeks to automatically pick
out the important parts in an image, and with this information (data), we can
determine a much more human-like assessment of the quality of an image.
%}