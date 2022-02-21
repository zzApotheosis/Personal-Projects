clc
close all
clear all

%% read your own image
X= imread('./standard_test_images/FJ1.jpg');
Xg=rgb2gray(X);
figure,imshow(Xg);
%imwrite(Xg,'./FJ_gray.tif');


%% get size and make a mask
[r,c]=size(Xg);
mask=rgb2gray(imread('./standard_test_images/FJ2.jpg'));%uint8(zeros(r,c));
figure,imshow(mask);

%% subtraction (remove the part you don't like)
sub=Xg-mask;
figure,imshow(sub);

%% a little bit analysis of the image
maxV=max(sub(:))
minV=min(sub(:))
hist(double(sub),50);

%% my first try of enhancement, I did increaced some pixels' value by 40
sub_e=sub;
for i=1:r
    for j=1:c
        
        if sub(i,j)>60 & sub(i,j)<120
            sub_e(i,j)=sub(i,j)+40;
        end
    end
end
figure,imshow(sub_e);  % the result is not good

%% my second try of enhancement, I did gamma correction
sub_e2=imadjust(sub,[],[],0.2);
figure,imshow(sub_e2);


%% my last try, histogram eq
sub_e=histeq(sub);
figure,imshow(sub_e);


% noise=uint8(rand(r,c)*50);
% Xg_n=Xg+noise;
% figure,imshow(Xg_n);
% 
% 
% diff=Xg_n-Xg;
% figure,imshow(diff);


