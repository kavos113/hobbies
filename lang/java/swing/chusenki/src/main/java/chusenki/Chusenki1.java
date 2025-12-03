package chusenki;

import java.awt.Desktop;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Timer;

import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;

public class Chusenki1 extends JFrame implements ActionListener{
	Timer timer;
	JButton btn1,btn2,btn3,btn4;
	JLabel lbl1,lbl2;
	JPanel p;
	int sec = 0;
    static Cell[] cells = null;
    static String file = "chusenki.xlsx";
	
	public Chusenki1(String title) {
		this.setTitle(title);
		this.setBounds(50, 50, 800, 500);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		String value = JOptionPane.showInputDialog(this, "excelファイルの場所", file);
		file = value;
		
		btn1 = new JButton("スタート");
		btn1.setBounds(130, 250, 120, 40);
		btn1.addActionListener(this);
		btn1.setActionCommand("start");
		btn1.setFont(new Font("メイリオ",Font.BOLD,16));
		
		btn2 = new JButton("ストップ");
		btn2.setBounds(270, 250, 120, 40);
		btn2.addActionListener(this);
		btn2.setActionCommand("stop");
		btn2.setFont(new Font("メイリオ",Font.BOLD,16));
		btn2.setEnabled(false);
		
		btn3 = new JButton("リセット");
		btn3.setBounds(410, 250, 120, 40);
		btn3.addActionListener(this);
		btn3.setActionCommand("reset");
		btn3.setFont(new Font("メイリオ",Font.BOLD,16));
		
		btn4 = new JButton("変更");
		btn4.setBounds(550, 250, 120, 40);
		btn4.addActionListener(this);
		btn4.setActionCommand("change");
		btn4.setFont(new Font("メイリオ",Font.BOLD,16));
		
		lbl1 = new JLabel("ここに表示します");
		lbl1.setBounds(0, 290, 800, 190);
        lbl1.setFont(new Font("メイリオ",Font.BOLD,80));
        lbl1.setHorizontalAlignment(JLabel.CENTER);
        
        lbl2 = new JLabel("抽選機");
		lbl2.setBounds(0, 50, 800, 190);
        lbl2.setFont(new Font("メイリオ",Font.BOLD,140));
        lbl2.setHorizontalAlignment(JLabel.CENTER);
		
		p = new JPanel();
		p.setLayout(null);
		p.add(btn1);
		p.add(btn2);
		p.add(btn3);
		p.add(btn4);
		p.add(lbl1);
		p.add(lbl2);
		getContentPane().add(p);
		
		timer = new Timer(50,this);
		timer.setActionCommand("timer");
	}

	public static void main(String[] args) {
		Chusenki1 frame = new Chusenki1("抽選機");
		frame.setVisible(true);
		
        FileInputStream fis = null;
        Workbook wb = null;
        
        try {
        	fis = new FileInputStream(file);
        	wb = WorkbookFactory.create(fis);
        }catch(IOException e){
        	System.out.println(e.toString());
        }catch(EncryptedDocumentException e) {
        	System.out.println(e.toString());
        }finally {
        	try {
        		fis.close();
        	}catch(IOException e){
            	System.out.println(e.toString());
        	}	
        }
        
        Sheet sheet = wb.getSheetAt(0);
        int rowlength = sheet.getLastRowNum();
        System.out.println(rowlength + 1);
        cells = new Cell[rowlength + 1];
        for(int i = 0; i < ( rowlength + 1 ); i++) {
        	Row row = sheet.getRow(i);
        	Cell cell = row.getCell(0);
        	cells[i] = cell;
        }
	}
	
	public void actionPerformed(ActionEvent e) {
		String cmd = e.getActionCommand();
		
		if(cmd.equals("start")) {
			btn1.setEnabled(false);
			btn2.setEnabled(true);
			
			timer.start();
		}else if(cmd.equals("stop")) {
			btn2.setEnabled(false);
			btn1.setEnabled(true);
			
			timer.stop();
		}else if(cmd.equals("timer")) {
    		lbl1.setText(cells[sec].getStringCellValue());
    		if(sec >= ( cells.length - 1 )) {
    			sec = sec - cells.length + 1;
    		}else {
    			sec++;
    		}	
		}else if(cmd.equals("reset")) {
			btn2.setEnabled(false);
			btn1.setEnabled(true);
			
			timer.stop();
			sec = 0;
			lbl1.setText("ここに表示します");
		}else if(cmd.equals("change")) {
			File file1 = new File(file);
			
			Desktop ds = Desktop.getDesktop();
			try {
				ds.edit(file1);
			}catch(IOException f){
				System.out.println(f.toString());
			}
		}
	}

}
