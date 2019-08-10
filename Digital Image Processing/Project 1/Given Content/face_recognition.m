%%% face recognition by Kalyan Sourav Dash %%%
clear all
close all
clc

%%%%%%%  provide the data path where the training images are present  %%%%%%%
%%% if your matlab environment doesn't support 'uigetdir' function
%%% change those lines in code for datapath and testpath as :
% datapath = 'give here the path of your training images';
% testpath = 'similarly give the path for test images';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
datapath = uigetdir(".", 'select path of training images');
testpath = uigetdir(".", 'select path of test images');

prompt = {'Enter test image name (a number between 1 to 10):'};
dlg_title = 'Input of PCA-Based Face Recognition System';
num_lines= 1;
def = {' '};
% TestImage = inputdlg(prompt,dlg_title,num_lines,def);
TestImage = strcat(testpath, "/1.jpg");
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%  calling the functions  %%%%%%%%%%%%%%%%%%%%%%%%

recog_img = facerecog(datapath, TestImage);
selected_img = strcat(datapath,'\',recog_img);
select_img = imread(selected_img);
imshow(select_img);
title('Recognized Image');
test_img = imread(TestImage);
figure,imshow(test_img);
title('Test Image');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
result = strcat('the recognized image is : ',recog_img);
disp(result);
