%{
Created by Steven Jennings on 26 January 2018.
Homework 1
%}

% Clean up workspace
clc;
clear;
close all;

% Prepare raw image data
original_rgb = imread("./input.png");
original_gray = uint8(rgb2gray(original_rgb));
mask = imread("./mask.jpg");

% Set resolution variables from image data
[rows, cols] = size(original_gray);

% Create other variables
temp = uint8(zeros([rows, cols])); % Temporary matrix for general tasks
threshold = 110;

% Prepare resulting image matrix
result = uint8(zeros([rows, cols]));

% Basic Image Analysis
max_val = max(original_gray(:));
min_val = min(original_gray(:));
mean_val = mean(original_gray(:));
%imhist(original_gray);

% ******************************************************************************

% Write grayscaled original image
imwrite(original_gray, "output_0.jpg");

% ******************************************************************************

% Process Image (Background removal)
for i = 1:rows
  for j = 1:cols
    if mask(i, j) < 5 % Low value in case of any "bad" values in the mask
      result(i, j) = original_gray(i, j);
    else
      result(i, j) = 255;
      temp(i, j) = original_gray(i, j); % Put background values into temp
    end
  end
end

% Write output_1
imwrite(result, "output_1.jpg");

% ******************************************************************************

% Adjust background gamma
temp = imadjust(temp, [], [], 2.0); % Adjusting the background gamma

% Create silhouette foreground with adjusted background
for i = 1:rows
  for j = 1:cols
    if mask(i, j) < 5
      result(i, j) = 30; % Low value for a dark-colored silhouette
    else
      result(i, j) = temp(i, j); % Gamma-adjusted background values
    end
  end
end

% Write output_2
imwrite(result, "output_2.jpg");

% ******************************************************************************

% Original foreground with gamma-adjusted background
for i = 1:rows
  for j = 1:cols
    if mask(i, j) < 5
      result(i, j) = original_gray(i, j);
    else
      result(i, j) = temp(i, j);
    end
  end
end

% Write output_3
imwrite(result, "output_3.jpg");

% ******************************************************************************

% Use thresholding to create a binary image
for i = 1:rows
  for j = 1:cols
    if original_gray(i, j) > threshold
      result(i, j) = 255;
    else
      result(i, j) = 0;
    end
  end
end

% Write output_4
imwrite(result, "output_4.jpg");

% ******************************************************************************

% Announce program end
disp("Done."); % Could also use printf("Done.\n");