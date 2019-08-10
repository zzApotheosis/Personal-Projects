%{
Created by Steven Jennings on 5 March 2018.

This is Part 2 of the assignment.

DO NOT RUN THIS FILE.
%}

% Clean up workspace
% close all
% clear all
% clc

% Create "reference image" of 0.5 values
reference = ones([x, y]) ./ 2;

% Declare variables
noise_gauss = [];
noise_uniform = [];
noise_snp = [];

% Generate Gaussian Noise
noise_gauss = imnoise(reference, "gaussian", 0, 0.01);

% Generate Uniform Noise
noise_uniform = reference .+ unifrnd(-0.05, 0.05, [x, y]);

% Generate Salt & Pepper Noise
noise_snp = imnoise(reference, "salt & pepper", 0.02);

% Write noise to persistent files
imwrite(noise_gauss, "./Noise Signals/noise_gauss.jpg");
imwrite(noise_uniform, "./Noise Signals/noise_uniform.jpg");
imwrite(noise_snp, "./Noise Signals/noise_snp.jpg");