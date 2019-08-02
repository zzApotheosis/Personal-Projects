import java.io.IOException;

import com.pi4j.io.spi.SpiChannel;
import com.pi4j.io.spi.SpiDevice;
import com.pi4j.io.spi.SpiFactory;

public class RFID {
	// Object to handle SPI transactions
	private SpiDevice spi = null;

	// Object to hold registers and values
	private MFRC522 values = new MFRC522();

	public RFID() throws IOException {
		// Default SPI speed: 1MHz
		// Default SPI mode: 0
		this.spi = SpiFactory.getInstance(SpiChannel.CS0, SpiDevice.DEFAULT_SPI_SPEED, SpiDevice.DEFAULT_SPI_MODE);

		Init();
	}

	private void Init() {
		Reset();
		// NoIRQ();

		SPIWrite(values.TModeReg, (byte) 0x8D);
		SPIWrite(values.TPrescalerReg, (byte) 0x3E);
		SPIWrite(values.TReloadRegL, (byte) 30);
		SPIWrite(values.TReloadRegH, (byte) 0);
		SPIWrite(values.TxAutoReg, (byte) 0x40);
		SPIWrite(values.ModeReg, (byte) 0x3D);
		AntennaOn();
	}

	private byte SPIRead(byte register) {
		byte buffer[] = new byte[2];
		buffer[0] = (byte) (((register << 1) & 0x7E) | 0x80);
		buffer[1] = 0; // Dummy data
		try {
			buffer = spi.write(buffer);
		} catch (IOException e) {
			System.out.println("SPIRead failed");
			System.out.println("Target address: " + register);
			e.printStackTrace();
		}
		return buffer[1];
	}

	private void SPIWrite(byte register, byte data) {
		byte buffer[] = new byte[2];
		buffer[0] = (byte) ((register << 1) & 0x7E);
		buffer[1] = data;
		try {
			spi.write(buffer);
		} catch (IOException e) {
			System.out.println("SPIWrite failed");
			System.out.println("Target address: " + register);
			System.out.println("Data: " + data);
			e.printStackTrace();
		}
	}

	private void Reset() {
		SPIWrite(values.CommandReg, values.PCD_RESETPHASE);
	}

	private void AntennaOn() {
		byte value = SPIRead(values.TxControlReg);
		// if ((value & 0x03) != 0x03)
		SetBitMask(values.TxControlReg, (byte) 0x03);
	}

	private void AntennaOff() {
		ClearBitMask(values.TxControlReg, (byte) 0x03);
	}

	private void SetBitMask(byte register, byte mask) {
		byte value = SPIRead(register);
		SPIWrite(register, (byte) (value | mask));
	}

	private void ClearBitMask(byte register, byte mask) {
		byte value = SPIRead(register);
		SPIWrite(register, (byte) (value & (~mask)));
	}

	private int Write_Card(byte command, byte[] data, int dataLen, byte[] back_data, int[] back_bits, int[] backLen) {
		int status = values.MI_ERR;
		byte irq = 0, irq_wait = 0, lastBits = 0;
		int n = 0, i = 0;

		backLen[0] = 0;
		if (command == values.PCD_AUTHENT) {
			irq = 0x12;
			irq_wait = 0x10;
		} else if (command == values.PCD_TRANSCEIVE) {
			irq = 0x77;
			irq_wait = 0x30;
		}

		SPIWrite(values.CommIEnReg, (byte) (irq | 0x80));
		ClearBitMask(values.CommIrqReg, (byte) 0x80);
		SetBitMask(values.FIFOLevelReg, (byte) 0x80);

		SPIWrite(values.CommandReg, values.PCD_IDLE);

		for (i = 0; i < dataLen; i++)
			SPIWrite(values.FIFODataReg, data[i]);

		SPIWrite(values.CommandReg, command);
		if (command == values.PCD_TRANSCEIVE)
			SetBitMask(values.BitFramingReg, (byte) 0x80);

		i = 2000;
		while (true) { // Something weird is happening here...
			n = SPIRead(values.CommIrqReg);
			i--;
			if ((i == 0) || (n & 0x01) > 0 || (n & irq_wait) > 0) {
				// System.out.println("Write_Card i = " + i + ", n = " + n); // Debug
				break;
			}
		}
		ClearBitMask(values.BitFramingReg, (byte) 0x80);

		if (i != 0) {
			if ((SPIRead(values.ErrorReg) & 0x1B) == 0x00) {
				status = values.MI_OK;
				// System.out.println("1 => Setting status to: " + status); // Debug
				if ((n & irq & 0x01) > 0) {
					status = values.MI_NOTAGERR;
					// System.out.println("2 => Setting status to: " + status); // Debug
				}
				if (command == values.PCD_TRANSCEIVE) {
					n = SPIRead(values.FIFOLevelReg);
					lastBits = (byte) (SPIRead(values.ControlReg) & 0x07);
					if (lastBits != 0)
						back_bits[0] = (n - 1) * 8 + lastBits;
					else
						back_bits[0] = n * 8;

					if (n == 0)
						n = 1;
					if (n > values.MAX_LEN)
						n = values.MAX_LEN;
					backLen[0] = n;
					for (i = 0; i < n; i++)
						back_data[i] = SPIRead(values.FIFODataReg);
				}
			} else {
				status = values.MI_ERR;
				// System.out.println("3 => Setting status to: " + status); // Debug
			}
		}
		return status;
	}

	public int Request(byte req_mode, int[] back_bits) {
		int status;
		byte tagType[] = new byte[1];
		byte data_back[] = new byte[16];
		int backLen[] = new int[1];

		SPIWrite(values.BitFramingReg, (byte) 0x07);

		tagType[0] = req_mode;
		back_bits[0] = 0;
		status = Write_Card(values.PCD_TRANSCEIVE, tagType, 1, data_back, back_bits, backLen);
		if (status != values.MI_OK || back_bits[0] != 0x10) {
			// System.out.println("Under Request(): status = " + status); // Debug
			// System.out.println("Under Request(): back_bits[0] = " + back_bits[0]); // Debug
			status = values.MI_ERR;
		}

		return status;
	}

	public int AntiColl(byte[] back_data) {
		int status;
		byte[] serial_number = new byte[2]; // 2�ֽ�����
		int serial_number_check = 0;
		int backLen[] = new int[1];
		int back_bits[] = new int[1];
		int i;

		SPIWrite(values.BitFramingReg, (byte) 0x00);
		serial_number[0] = values.PICC_ANTICOLL;
		serial_number[1] = 0x20;
		status = Write_Card(values.PCD_TRANSCEIVE, serial_number, 2, back_data, back_bits, backLen);
		if (status == values.MI_OK) {
			if (backLen[0] == 5) {
				for (i = 0; i < 4; i++)
					serial_number_check ^= back_data[i];
				if (serial_number_check != back_data[4]) {
					status = values.MI_ERR;
					System.out.println("check error");
				}
			} else {
				// I noticed the program ran this code when there were two or more cards in the field.
				status = values.MI_OK;
				// System.out.println("backLen[0]=" + backLen[0]); // Debug
			}
		}
		return status;
	}

	// It looks like this uses a byte[] object as a pointer to send data back to the "data" argument
	// in this method
	private void Calculate_CRC(byte[] data) {
		int i, n;
		ClearBitMask(values.DivIrqReg, (byte) 0x04);
		SetBitMask(values.FIFOLevelReg, (byte) 0x80);

		for (i = 0; i < data.length - 2; i++)
			SPIWrite(values.FIFODataReg, data[i]);
		SPIWrite(values.CommandReg, values.PCD_CALCCRC);
		i = 255;
		while (true) {
			n = SPIRead(values.DivIrqReg);
			i--;
			if ((i == 0) || ((n & 0x04) > 0))
				break;
		}
		data[data.length - 2] = SPIRead(values.CRCResultRegL);
		data[data.length - 1] = SPIRead(values.CRCResultRegH);
	}

	public int Select_Tag(byte[] uid) {
		int status;
		byte data[] = new byte[9];
		byte back_data[] = new byte[values.MAX_LEN];
		int back_bits[] = new int[1];
		int backLen[] = new int[1];
		int i, j;

		data[0] = values.PICC_SElECTTAG;
		data[1] = 0x70;
		for (i = 0, j = 2; i < 5; i++, j++)
			data[j] = uid[i];
		Calculate_CRC(data);

		status = Write_Card(values.PCD_TRANSCEIVE, data, 9, back_data, back_bits, backLen);
		if (status == values.MI_OK && back_bits[0] == 0x18)
			return back_data[0];
		else
			return 0;
	}

	public int Auth_Card(byte auth_mode, byte block_address, byte[] key, byte[] uid) {
		int status;
		byte data[] = new byte[12];
		byte back_data[] = new byte[values.MAX_LEN];
		int back_bits[] = new int[1];
		int backLen[] = new int[1];
		int i, j;

		data[0] = auth_mode;
		data[1] = block_address;
		for (i = 0, j = 2; i < 6; i++, j++)
			data[j] = key[i];
		for (i = 0, j = 8; i < 4; i++, j++)
			data[j] = uid[i];

		status = Write_Card(values.PCD_AUTHENT, data, 12, back_data, back_bits, backLen);
		if ((SPIRead(values.Status2Reg) & 0x08) == 0)
			status = values.MI_ERR;
		return status;
	}
	
	public int Auth_Card(byte auth_mode, byte sector, byte block, byte[] key, byte[] uid) {
		return Auth_Card(auth_mode, Sector2BlockAddress(sector, block), key, uid);
	}

	// Ends operations with Crypto1 usage.
	public void Stop_Crypto() {
		ClearBitMask(values.Status2Reg, (byte) 0x08);
	}

	// Reads data from block. You should be authenticated before calling read.
	// Returns tuple of (result state, read data).
	// block_address
	// back_data-data to be read,16 bytes
	public int ReadBlock(byte block_address, byte[] back_data) {
		int status;
		byte data[] = new byte[4];
		int back_bits[] = new int[1];
		int backLen[] = new int[1];
		// int i, j;

		data[0] = values.PICC_READ;
		data[1] = block_address;
		Calculate_CRC(data);
		status = Write_Card(values.PCD_TRANSCEIVE, data, data.length, back_data, back_bits, backLen);
		if (backLen[0] == 16)
			status = values.MI_OK;
		return status;
	}

	public int ReadBlock(byte sector, byte block, byte[] back_data) {
		return ReadBlock(Sector2BlockAddress(sector, block), back_data);
	}

	// Writes data to block. You should be authenticated before calling write.
	// Returns error state.
	// data-16 bytes
	public int WriteBlock(byte block_address, byte[] data) {
		int status;
		byte buff[] = new byte[4];
		byte buff_write[] = new byte[data.length + 2];
		byte back_data[] = new byte[values.MAX_LEN];
		int back_bits[] = new int[1];
		int backLen[] = new int[1];
		int i;

		buff[0] = values.PICC_WRITE;
		buff[1] = block_address;
		Calculate_CRC(buff);
		status = Write_Card(values.PCD_TRANSCEIVE, buff, buff.length, back_data, back_bits, backLen);
		// System.out.println("write_card  status="+status);
		// System.out.println("back_bits[0]="+back_bits[0]+",(back_data[0] & 0x0F)="+(back_data[0] & 0x0F));
		if (status != values.MI_OK || back_bits[0] != 4 || (back_data[0] & 0x0F) != 0x0A)
			status = values.MI_ERR;
		if (status == values.MI_OK) {
			for (i = 0; i < data.length; i++)
				buff_write[i] = data[i];
			Calculate_CRC(buff_write);
			status = Write_Card(values.PCD_TRANSCEIVE, buff_write, buff_write.length, back_data, back_bits, backLen);
			// System.out.println("write_card data status="+status);
			// System.out.println("back_bits[0]="+back_bits[0]+",(back_data[0] & 0x0F)="+(back_data[0] & 0x0F));
			if (status != values.MI_OK || back_bits[0] != 4 || (back_data[0] & 0x0F) != 0x0A)
				status = values.MI_ERR;
		}
		return status;
	}

	public int WriteBlock(byte sector, byte block, byte[] data) {
		return WriteBlock(Sector2BlockAddress(sector, block), data);
	}

	// Convert sector to blockaddress
	// sector-0~15
	// block-0~3
	// return blockaddress
	private byte Sector2BlockAddress(byte sector, byte block) {
		if (sector < 0 || sector > 15 || block < 0 || block > 3)
			return (byte) (-1); // If someone tries to access a sector or block that doesn't exist
		return (byte) (sector * 4 + block);
	}

	public int Select_MiFareOne(byte[] uid) {
		int back_bits[] = new int[1];
		byte tagid[] = new byte[5];
		int status;

		status = Request(values.PICC_REQIDL, back_bits);
		if (status != values.MI_OK) {
			// System.out.println("Request() failed under Select_MiFareOne"); // Debug
			return status;
		}
		status = AntiColl(tagid);
		if (status != values.MI_OK) {
			// System.out.println("AntiColl() failed under Select_MiFareOne"); // Debug
			return status;
		}
		Select_Tag(tagid);
		System.arraycopy(tagid, 0, uid, 0, 5);

		return status;
	}

	// Get all data from MiFare card
	public byte[] DumpClassic1K(byte[] key, byte[] uid) {
		int i, status;
		byte[] data = new byte[1024];
		byte[] buff = new byte[16];

		for (i = 0; i < 64; i++) {
			status = Auth_Card(values.PICC_AUTHENT1A, (byte) i, key, uid);
			if (status == values.MI_OK) {
				status = ReadBlock((byte) i, buff);
				if (status == values.MI_OK)
					System.arraycopy(buff, 0, data, i * 64, 16);
			}
		}
		return data;
	}

	public String bytesToHex(byte[] bytes) {
		final char[] hexArray = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
		char[] hexChars = new char[bytes.length * 2];
		int v;
		for (int j = 0; j < bytes.length; j++) {
			v = bytes[j] & 0xFF;
			hexChars[j * 2] = hexArray[v >>> 4];
			hexChars[j * 2 + 1] = hexArray[v & 0x0F];
		}
		return new String(hexChars);
	}
}