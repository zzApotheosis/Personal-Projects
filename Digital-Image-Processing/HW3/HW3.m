%{
Created by Steven Jennings on 25 February 2018.
%}

% Clean up workspace
clc
clear all
close all

% Fetch input data
face_dark_rgb = imread("face_dark.bmp");
length_dark = size(face_dark_rgb); % Size of "dark" image

% Convert to HSV (Normalized values)
face_dark_hsv = rgb2hsv(face_dark_rgb);

%{
PART 1 *************************************************************************

First, we work with the bad image.
%}

% Histograms of bad image
histogram_h = imhist(face_dark_hsv(1:end, 1:end, 1)); % Hue
histogram_s = imhist(face_dark_hsv(1:end, 1:end, 2)); % Saturation
histogram_v = imhist(face_dark_hsv(1:end, 1:end, 3)); % Value

% Figure 1: Show histograms of HSV colorspace
figure;
subplot(2,2,1); imshow(face_dark_rgb); title("Original Image"); 
subplot(2,2,2); plot(histogram_h); title("Hue");
subplot(2,2,3); plot(histogram_s); title("Saturation");
subplot(2,2,4); plot(histogram_v); title("Value");

% Create binary images
binH = [];
binS = [];
binV = [];

% Threshold Hue within range
lower_limit = 0;
upper_limit = 20;
for i = 1:length_dark(1)
  for j = 1:length_dark(2)
    if lower_limit / 256 < face_dark_hsv(i, j, 1) && face_dark_hsv(i, j, 1) < upper_limit / 256
      binH(i, j) = true;
    else
      binH(i, j) = false;
    end
  end
end

% Threshold Saturation within range
lower_limit = 60;
upper_limit = 220;
for i = 1:length_dark(1)
  for j = 1:length_dark(2)
    if lower_limit / 256 < face_dark_hsv(i, j, 2) && face_dark_hsv(i, j, 2) < upper_limit / 256
      binS(i, j) = true;
    else
      binS(i, j) = false;
    end
  end
end

% Threshold Value within range
lower_limit = 100;
upper_limit = 227;
for i = 1:length_dark(1)
  for j = 1:length_dark(2)
    if lower_limit / 256 < face_dark_hsv(i, j, 3) && face_dark_hsv(i, j, 3) < upper_limit / 256
      binV(i, j) = true;
    else
      binV(i, j) = false;
    end
  end
end

% Clear temporary variables
clear lower_limit;
clear upper_limit;

% Figure 2: Show detected binary images
figure;
subplot(2,2,1); imshow(face_dark_rgb); title("Original Image");
subplot(2,2,2); imshow(binH); title("Binary Hue");
subplot(2,2,3); imshow(binS); title("Binary Saturation");
subplot(2,2,4); imshow(binV); title("Binary Value");

% AND all binary images
bin_im = binH & binS & binV;

% Figure 3: Show final binary image
figure;
imshow(bin_im); title("Binary Result");

% Create final image
result = face_dark_rgb;
for i = 1:length_dark(1)
  for j = 1:length_dark(2)
    if bin_im(i, j) == 1
      result(i, j, 1:end) = face_dark_rgb(i, j, 1:end);
    else
      result(i, j, 1:end) = 0;
    end
  end
end

% Write output
imwrite(result, "face_dark_detected.jpg");

%{
PART 2 *************************************************************************

Next, we work with the good image.

To separate the program, try putting a breakpoint on this line
(or anywhere between the first part and the second part).
%}

% Clean up workspace
clc
clear all
%close all

% Fetch input data
face_good_rgb = imread("face_good.bmp");
length_good = size(face_good_rgb); % Size of good image

% Convert to HSV (Normalized values)
face_good_hsv = rgb2hsv(face_good_rgb);

% Histograms of good image
histogram_h = imhist(face_good_hsv(1:end, 1:end, 1)); % Hue
histogram_s = imhist(face_good_hsv(1:end, 1:end, 2)); % Saturation
histogram_v = imhist(face_good_hsv(1:end, 1:end, 3)); % Value

% Figure 4: Show histograms of HSV colorspace
figure;
subplot(2,2,1); imshow(face_good_rgb); title("Original Image"); 
subplot(2,2,2); plot(histogram_h); title("Hue");
subplot(2,2,3); plot(histogram_s); title("Saturation");
subplot(2,2,4); plot(histogram_v); title("Value");

% Create binary images
binH = [];
binS = [];
binV = [];

% Threshold Hue within range
lower_limit = 0;
upper_limit = 80;
for i = 1:length_good(1)
  for j = 1:length_good(2)
    if lower_limit / 256 < face_good_hsv(i, j, 1) && face_good_hsv(i, j, 1) < upper_limit / 256
      binH(i, j) = true;
    else
      binH(i, j) = false;
    end
  end
end

% Threshold Saturation within range
lower_limit = 50;
upper_limit = 150;
for i = 1:length_good(1)
  for j = 1:length_good(2)
    if lower_limit / 256 < face_good_hsv(i, j, 2) && face_good_hsv(i, j, 2) < upper_limit / 256
      binS(i, j) = true;
    else
      binS(i, j) = false;
    end
  end
end

% Threshold Value within range
lower_limit = 100;
upper_limit = 200;
for i = 1:length_good(1)
  for j = 1:length_good(2)
    if lower_limit / 256 < face_good_hsv(i, j, 3) && face_good_hsv(i, j, 3) < upper_limit / 256
      binV(i, j) = true;
    else
      binV(i, j) = false;
    end
  end
end

% Clear temporary variables
clear lower_limit;
clear upper_limit;

% Figure 5: Show detected binary images
figure;
subplot(2,2,1); imshow(face_good_rgb); title("Original Image");
subplot(2,2,2); imshow(binH); title("Binary Hue");
subplot(2,2,3); imshow(binS); title("Binary Saturation");
subplot(2,2,4); imshow(binV); title("Binary Value");

% AND all binary images
bin_im = binH & binS & binV;

% Figure 6: Show final binary image
figure;
imshow(bin_im); title("Binary Result");

% Create final image
result = face_good_rgb;
for i = 1:length_good(1)
  for j = 1:length_good(2)
    if bin_im(i, j) == 1
      result(i, j, 1:end) = face_good_rgb(i, j, 1:end);
    else
      result(i, j, 1:end) = 0;
    end
  end
end

% Write output
imwrite(result, "face_good_detected.jpg");