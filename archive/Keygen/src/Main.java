/*
 * Created by Steven Jennings on 21 June 2019.
 */

import zzapo.JenningsUtil;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class Main {

    public static void main(String[] args) {
        DataHandler dataHandler = new DataHandler();
        dataHandler.run(parseArgs(args));
    }

    private static Type parseArgs(String[] in) {
        Type out = Type.NONE;
        if (in.length >= 1) {
            switch (in[0]) {
                case "-s":
                    out = Type.STANDARD;
                    break;
                case "-e":
                    out = Type.EXTRA_SECURE;
                    break;
                default:
                    out = Type.ERROR;
                    break;
            }
        }
        if (in.length > 1) {
            JenningsUtil.println("WARNING: Too many arguments. Valid options are -s for Standard Mode and -e for Extra Secure Mode");
        }
        return out;
    }

}
