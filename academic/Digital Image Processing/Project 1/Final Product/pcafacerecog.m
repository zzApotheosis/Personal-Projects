%{
Created by Steven Jennings on 25 March 2018.

I broke up my code into small blocks for readability. I also kept the workspace
very clean by clearing temporary variables like i, x, temp, etc., at the end of
each small block of code.
%}

% Clean up workspace
clc
clear all
close all
delete("./output/*");

%{
STEP 1: TRAINING
%}

% Set up for the problem
imlist = dir("./training/*.bmp");
im = imread(strcat("./training/", imlist(1).name)); % Just to fetch dimension information
[r, c] = size(im); % Dimension information, assuming every image is the same size as the first one
num_training_ims = length(imlist);
ims_per_person = 5; % Number of images per person
num_persons = num_training_ims / double(ims_per_person); % Number of people
ids = []; % Numerical IDs for each person in the training dataset
for i = 1:num_persons
  for j = 1:ims_per_person
    ids(ims_per_person * (i - 1) + j) = i;
  end
end
clear im;
clear i;
clear j;

% Vectorize all images
raw_training_data = [];
for i = 1:num_training_ims
  im = imread(strcat("./training/", imlist(i).name));
  raw_training_data(:, i) = im'(:);
end
clear im;
clear i;

% Calculate average values for EACH PERSON, store averages in their own columns
% You can view the average data of each person in their respective column
avg_persons = zeros(r * c, num_persons);
for i = 1:num_persons % i = person ID
    for j = 1:ims_per_person
    avg_persons(:, i) = avg_persons(:, i) + raw_training_data(:, ims_per_person * (i - 1) + j) ./ ims_per_person;
  end
end
clear i;
clear j;

% Calculate average image vector (Find common features between all faces)
avg_image_vector = mean(raw_training_data')(:);
avg_image = reshape(avg_image_vector', [r, c])';

% Subtract average person vector from each AVERAGE person image in the AVERAGE person matrix
mean_persons_minus_avg = [];
for i = 1:num_persons
  mean_persons_minus_avg(:, i) = avg_persons(:, i) .- avg_image_vector(:);
end
clear i;

% Calculate covariance matrix (under dimensionality reduction)
covariance_matrix = mean_persons_minus_avg' * mean_persons_minus_avg; % ata = A' * A

%{
NOTE: Computing in reduced dimensionality HEAVILY improves performance. This was
the "version 2" approach from the Day 16 pdf.
%}

% Calculate eigenvectors and eigenvalues (in reduced dimensionality)
[V_eigvec D_eigval] = eig(covariance_matrix);

% Get weights of training dataset
weights_training = V_eigvec' * covariance_matrix;

% Get actual eigenfaces
actual_eigenfaces = mean_persons_minus_avg * V_eigvec;

% Display all eigenfaces (Change true/false)
if false
  for i = 1:size(actual_eigenfaces)(2)
    figure;
    imagesc(reshape(actual_eigenfaces(:, i), [r, c])');
    colormap(gray(256));
    title(sprintf("Eigenface #%d", i));
    axis off;
    
    sleep(0.2);
  end
  
  clear i;
  return % I don't want this segment of code to interfere with the saveas() functions at the end
end

%{
STEP 2: TESTING
%}

imlist2 = dir("./testing/*.jpg");
num_testing_ims = length(imlist2);
image_training_vector = zeros(r * c, num_testing_ims);
match = zeros([30, num_testing_ims]);
cmc = zeros([30, num_testing_ims]);

% Stop program if there are no testing images
if num_testing_ims < 1
  disp("Need at least one testing image.");
  return
end

% Begin testing each individual image
for i = 1:num_testing_ims
  % Fetch test image
  im = imread(strcat("./testing/", imlist2(i).name));
  image_training_vector(:, i) = reshape(im', [r * c, 1]); % Add test image to total testing matrix
  
  % Get mean-subtracted test image
  test_image_minus_avg(:, i) = image_training_vector(:, i) .- avg_image_vector;
  
  % Calculate weights of the mean-subtracted test image
  weights_testing = actual_eigenfaces' * test_image_minus_avg(:, i);
  
  % Calculate euclidean distance (on every training image)
  for j = 1:num_persons
    eud(j, i) = sqrt(sum((weights_testing - weights_training(:, j)) .^ 2));
  end
  
  % Find minimum euclidean distance
  [minimum_data, matched_index(i)] = min(eud(:, i));
  
  % Collect ranked matches
  [svals, idx] = sort(eud(:, i));
  for k = 1:30
    if idx(k) == ids(i)
      match(k, i) = match(k, i) + 1;
    end
  end
end
clear i;
clear j;
clear k;
clear svals;
clear idx;
clear minimum_data;
clear weights_testing;

% Accumulate the CMC data
for i = 1:30
  for j = 1:num_testing_ims
    cmc(i, j) = sum(match(1:i, j));
  end
end
clear i;
clear j;

% Plot all CMC curves
for i = 1:num_testing_ims
  figure;
  plot(cmc(:, i));
  title(sprintf("CMC Curve %d", i));
  
  pause(0.2);
end
clear i;

% Display all matched images
for i = 1:num_testing_ims
  figure;
  
  subplot(2, 2, 1);
  imagesc(imread(strcat("./testing/", imlist2(i).name)));
  colormap(gray(256));
  axis off;
  
  subplot(2, 2, 2);
  imagesc(imread(strcat("./training/", imlist(matched_index(i) * ims_per_person).name)));
  colormap(gray(256));
  axis off;
  
  pause(0.2);
end
clear i;

% Save figures
for i = 1:(2 * num_testing_ims)
  saveas(i, sprintf("./output/%d.png", i), "png");
end

return % END SCRIPT HERE *******************************************************





%{
ALL THE CODE BELOW IS MOSTLY JUST EXPERIMENTING. All of my final code is above.
%}






























% Confused from this point *******************************

% Fetch diagonal of D_eigval (Don't really know what it's for)
x = diag(D_eigval);
[xc xci] = sort(x, "descend");

% Write all eigenfaces to files
disp("Writing eigenfaces...");
for i = 1:num_persons
  imwrite(reshape(normal_eigenfaces(:, i), [r, c])', sprintf("./output/eigenfaces/Eigenface %d.jpg", i));
end
clear i;
disp("Done");

% Calculate weights????? (Confused here)
weights_training = [];
k = 10; % Select eigenfaces
for i = 1:num_training_ims % Image number
  for j = 1:k % Eigenface for coefficient number (In reverse order)
    weights_training(i, j) = sum(data_minus_avg(:, i) .* normal_eigenfaces(:, xci(j)));
  end
end
clear i;
clear j;

% Subtract database average from test image
test_im_minus_avg = test_im .- avg_image;

% Calculate weights?
testingweights = reshape(normal_eigenfaces(:, 121), [r, c])' * test_im_minus_avg;








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
























%% BELOW IS DR. JIANG'S SAMPLE CODE

clc
clear all
close all

%%%%%%%%%%%%%%%%%%%%% TRAINING %%%%%%%%%%%%%%%%%%%%
imlist = dir('./training/*.bmp');
im = imread(strcat('./training/', imlist(1).name));
[r, c] = size(im);
num_im = length(imlist);
num_p = num_im / 2;
x =         zeros(r * c, num_p);
im_vector = zeros(r * c, num_im);
Mec =       zeros(r * c, 1);
index =     zeros;
index2 =    zeros;
match =     zeros(1, 10); match2 = zeros(1, 10);
cmc =       zeros(1, 10);
cmc2 =      zeros(1, 10);

%%%%%% convert all images to vector %%%%%%
for i = 1:num_im
  im = imread(['./training/', imlist(i).name]);
  im_vector(:, i) = reshape(im', r * c, 1);
end

%%%%%%%%%%%%%% to get xi and Me%%%%%%%%%%%%%%%%
j = 1;
for i = 1:2:(num_im - 1)
  x(:, j) = (im_vector(:, i) + im_vector(:, i + 1)) ./ 2;
  Mec(:, 1) = Mec(:, 1) + im_vector(:, i) + im_vector(:, i + 1);
  j = j + 1;
end
Me = Mec(:, 1) ./ num_im;

%%%%%%%%%%%%%% to get big A %%%%%%%%%%%%%%%%%%%%
for i = 1:num_p
  a(:, i) = x(:, i) - Me;
end

%%%%%%%%%%%%%% to get eig of A'*A (P2) %%%%%%%%%
% NOTE: A' * A is the covariance matrix in lower dimensional space
% A * A' would give the covariance matrix in face vector space

ata = a' * a;  
[V D] = eig(ata);
p2 = [];
for i = 1:size(V, 2) 
  if (D(i,i) > 1)
    p2 = [p2 V(:, i)];
  end
end

%%%weight of the training data projected into eigen space%%%%%
wta = p2' * ata;

%% Plot the weights for person 1 2 3 4...
figure;
plot(wta(:, 20));

%%%%%%%%%%%%%% to get the Eigenfaces %%%%%%%%%%%%
ef = a * p2;  %here is P you need to use in matching 
[rr, cc] = size(ef);

%%show the eigenface images ...
temp = reshape(ef(:, 110), [100 100])';

figure;
imshow(temp);

%% %%%%%%%%%%%%%%%%%%%%%  TESTING  %%%%%%%%%%%%%%%%%%%%%%%%
imlist2 = dir('./testing/*.png');
num_imt = length(imlist2);
imt_vector = zeros(r * c, num_imt);

%%%%%% convert all test images to vector %%%%%%
for i = 1:num_imt
im = imread(['./testing/', imlist2(i).name]);
imt_vector(:, i) = reshape(im', r * c, 1);

%%%%% get B=y-me %%%%%%%
   %% bi=imt_vector(i)-Me;
   %%wtb=P'*bi;
for ii = 1:num_p   %% weight compare wtb and wta(i)
 
end
   %%find minimum eud's index

%%%%%%%%%%%%%%%%%%%%%%%  RESULT  %%%%%%%%%%%%%%%%%%%%%%%%
%%% right result by observation is 1 1 2 3 4 %%%%%
result = [1 1 2 3 4];
%%%%%%%%%%%%%%% CMC calculation (compare with result)%%%%%%%



end

%%%%%%%%%%%%%%% CMC curve plot %%%%%%%
 
 



i=i;