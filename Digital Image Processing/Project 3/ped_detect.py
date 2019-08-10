# USAGE
# python detect.py

# Modified by Steven Jennings

# IMPORTANT NOTE: You must have the following modules installed (some of them
# should be default standard libraries anyway):
# OpenCV (cv2)
# numpy
# imutils
# shutil

# import the necessary packages
from __future__ import print_function
from imutils.object_detection import non_max_suppression
from imutils import paths
import sys
import numpy as np
import argparse
import imutils
import cv2
import os
import shutil

class Main:

    x = None

    @staticmethod
    def main():
        # Create program control variables
        fps = 30 # Assuming 30 frames per second
        interval = 30 # Interval (in frames) at which to extract images
        offset = 0 # Offset for frame extraction
        duration = 60 * fps # Duration from the start of the starting frame
                            # to extract frames (in seconds * frames per second)

        # Announce program start
        Main.intro()

        # Clean up workspace
        Main.cleanup()

        # Fetch raw driving data (speed as determined by GPS)
        speed_data, time_data = Main.getData()

        # Calculate first derivative of speed data (to determine time of potential crash)
        acceleration_data = Main.deriv(speed_data)

        # Determine starting frame of the video at which to extract frames
        start_frame = Main.getStartFrame(acceleration_data, time_data, fps)

        # start_frame = 5800 # Debug
        # Strip video input into separate frames
        Main.stripVideo(interval, offset, start_frame, duration)

        # Detect pedestrians
        Main.detectPedestrians()

    @staticmethod
    def intro():
        print("Starting Pedestrian Detection...")
        print("This automatically detects a crash from provided data"
            "\n(from sensors, for instance), and detects pedestrians from around the"
            "\npoint in the video where the crash happened.\n")

    @staticmethod
    def cleanup():
        # Clean up workspace
        in_dir = "./input"
        im_dir = "./images"
        out_dir = "./output"

        # Check for output and image directories
        if not os.path.exists(out_dir):
            os.makedirs(out_dir)
        if not os.path.exists(im_dir):
            os.makedirs(im_dir)

        # Clear out image and output directories
        for file in os.listdir(out_dir):
            file_path = os.path.join(out_dir, file)
            try:
                if os.path.isfile(file_path):
                    os.unlink(file_path)
                elif os.path.isdir(file_path): shutil.rmtree(file_path)
            except Exception as e:
                print(e)
        for file in os.listdir(im_dir):
            file_path = os.path.join(im_dir, file)
            try:
                if os.path.isfile(file_path):
                    os.unlink(file_path)
                elif os.path.isdir(file_path): shutil.rmtree(file_path)
            except Exception as e:
                print(e)

        # Exit if the input folder does not exist
        if not os.path.exists(in_dir):
            os.makedirs(in_dir)
            print("Cannot analyze a null video file!\nPlace \"0.MOV\""
                "and \"000.dat\" in /input/")
            sys.exit(0)

    @staticmethod
    def getData():
        # Fetch data from file
        with open("./input/000.dat", "r") as f:
            data = f.readlines()

        # Organize into separate lists
        s = [] # Speed
        t = [] # Time
        for i in range(0, len(data)):
            if data[i][1] is "G":
                temp = data[i].split(" ")
                s.append(temp[5])
                t.append(temp[2])

        return s, t

    @staticmethod
    def deriv(s):
        d = []
        for i in range(0, len(s) - 1):
            d.append(float(s[i + 1]) - float(s[i]))
        return d

    @staticmethod
    def getStartFrame(a, t, fps):
        # Prepare the acceleration data using only the magnitude of the data (no negatives)
        magnitude = []
        for i in range(len(a)):
            magnitude.append(abs(a[i]))

        # Get ready to find maximum value in the acceleration data
        max_val = magnitude[0]
        index = 0

        # Determine value and index where a crash most likely happened
        for i in range(len(magnitude)):
            if magnitude[i] > max_val:
                max_val = magnitude[i]
                index = i

        # Get the initial time (when the video started)
        initial_time = t[0]
        x_i = initial_time.split(":")
        detected_time = t[index]
        x_d = detected_time.split(":")

        # Calculate time difference between detected time and initial time
        difference_time = []
        for i in range(len(x_d)):
            difference_time.append(float(x_d[i]) - float(x_i[i]))

        # Announce detection
        print("Detected potential crash at ", int(difference_time[0]), ":",
            int(difference_time[1]), ":", int(difference_time[2]), sep="")

        # Calculate the detected time in seconds
        detected_time_in_seconds = (3600 * difference_time[0] +
            60 * difference_time[1] + difference_time[2])

        # Calculate the approximate frame where the crash happened
        sf = detected_time_in_seconds * fps

        # Announce detected frame
        print("Potential crash is at frame", sf)

        return sf

    @staticmethod
    def stripVideo(interval=30, offset=0, start=0, duration=900):
        vc = cv2.VideoCapture("./input/0.MOV")
        c = 0

        if vc.isOpened():
            rval, frame = vc.read()
        else:
            rval = False

        while rval:
            rval, frame = vc.read()
            if (c + offset) % interval == 0 and c >= start:
                cv2.imwrite("./images/" + str(c) + '.bmp',frame)
            if c >= start + duration:
                break
            c += 1

        vc.release()

    @staticmethod
    def detectPedestrians():
        # initialize the HOG descriptor/person detector
        hog = cv2.HOGDescriptor()
        hog.setSVMDetector(cv2.HOGDescriptor_getDefaultPeopleDetector())

        im_dir = "./images"

        # loop over the image paths
        imagePaths = list(paths.list_images(im_dir))

        # i = 0
        for imagePath in imagePaths:
            # load the image
            image = cv2.imread(imagePath)
            # image = imutils.resize(image, width=min(800, image.shape[1]))
            orig = image.copy()

            # detect people in the image
            (rects, weights) = hog.detectMultiScale(image, winStride=(4, 4),
                                                    padding=(8, 8), scale=1.05)

            # draw the original bounding boxes
            for (x, y, w, h) in rects:
                cv2.rectangle(orig, (x, y), (x + w, y + h), (0, 0, 255), 2)

            # apply non-maxima suppression to the bounding boxes using a
            # fairly large overlap threshold to try to maintain overlapping
            # boxes that are still people
            rects = np.array([[x, y, x + w, y + h] for (x, y, w, h) in rects])
            pick = non_max_suppression(rects, probs=None, overlapThresh=0.50)

            # draw the final bounding boxes
            for (xA, yA, xB, yB) in pick:
                cv2.rectangle(image, (xA, yA), (xB, yB), (0, 255, 0), 2)

            # show some information on the number of bounding boxes
            filename = imagePath[imagePath.rfind("/") + 1:]
            print("[INFO] {}: {} original boxes, {} after suppression".format(
                filename, len(rects), len(pick)))

            # show the output images
            # cv2.imshow("Before NMS", orig)
            # cv2.imshow("After NMS", image)
            # cv2.imwrite("./output/" + str(i) + "_orig.bmp", orig)
            cv2.imwrite("./output/" + filename, image)
            # cv2.waitKey(0)


if __name__ == "__main__":
    Main.main()
