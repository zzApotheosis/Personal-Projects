%{
Created by Steven Jennings on 5 March 2018.

This is Part 1 of the assignment.

ONLY RUN THIS FILE. This file will execute the other parts.
%}

% Clean up workspace
close all
clear all
clc

% Declare variables
x = 256;
y = 256;
center = [x / 2, y / 2]; % Perfect center
radius = 80;

% Begin Image Generation

% Declare image (Double)
im = [];

% Populate values into matrix
for i = 1:x
  for j = 1:y
    if sqrt((i - center(1)) ^ 2 + (j - center(2)) ^ 2) < radius
      im(i, j) = 0.4;
    else
      im(i, j) = 0.7;
    end
  end
end

% Write image to persistent file
imwrite(im, "original_image.jpg");

% Run parts in succession
run "HW4.2"
run "HW4.3"
run "HW4.4"