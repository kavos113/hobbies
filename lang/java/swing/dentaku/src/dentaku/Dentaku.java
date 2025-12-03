package dentaku;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

public class Dentaku extends JFrame implements ActionListener{
	
	static JButton btnp,btnm,btnk,btnw,btni,btpm,btpo,btna,btnc,btnr;
	static JButton[] btn = new JButton[10];
 	JPanel p;
	JLabel l1,l2;
	static CalcEngine ce;
	int count = 0;
	boolean flag = false;
	static int hei,wid;
	Timer timer;
	static Dentaku de;

	public static void main(String[] args) {
		ce = new CalcEngine();
		de = new Dentaku();
		
		de.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		de.setBounds(100, 100, 340, 490);
		de.setVisible(true);
        hei = de.getHeight() / 7;
    	wid = de.getWidth() / 4;
    	de.setMinimumSize(new Dimension(340,490));
	}
	
	public Dentaku() {
		
		timer = new Timer(30,this);
		timer.start();
		timer.setActionCommand("timer");
		
		
		for(int i = 0; i < 10; i++) {
			btn[i] = new JButton(String.valueOf(i));
			btn[i].setFont(new Font("メイリオ",Font.BOLD,40));
			btn[i].addActionListener(this);
			btn[i].setActionCommand(String.valueOf(i));
		}
		
		btn[1].setBounds(0, 200, 80, 80);
		btn[2].setBounds(80, 200, 80, 80);
		btn[3].setBounds(160, 200, 80, 80);
		btn[4].setBounds(0, 280, 80, 80);
		btn[5].setBounds(80, 280, 80, 80);
		btn[6].setBounds(160, 280, 80, 80);
		btn[7].setBounds(0, 360, 80, 80);
		btn[8].setBounds(80, 360, 80, 80);
		btn[9].setBounds(160, 360, 80, 80);
		btn[0].setBounds(80, 440, 80, 80);
		
		btnp = new JButton("+");
		btnp.setBounds(240, 120, 80, 80);
		btnp.setFont(new Font("メイリオ",Font.BOLD,40));
		btnp.addActionListener(this);
		btnp.setActionCommand("+");
		
		btnm = new JButton("-");
		btnm.setBounds(240, 200, 80, 80);
		btnm.setFont(new Font("メイリオ",Font.BOLD,40));
		btnm.addActionListener(this);
		btnm.setActionCommand("-");
		
		btnk = new JButton("x");
		btnk.setBounds(240, 280, 80, 80);
		btnk.setFont(new Font("メイリオ",Font.BOLD,40));
		btnk.addActionListener(this);
		btnk.setActionCommand("x");
		
		btnw = new JButton("÷");
		btnw.setBounds(240, 360, 80, 80);
		btnw.setFont(new Font("メイリオ",Font.BOLD,40));
		btnw.addActionListener(this);
		btnw.setActionCommand("÷");
		
		btni = new JButton("=");
		btni.setBounds(240, 440, 80, 80);
		btni.setFont(new Font("メイリオ",Font.BOLD,40));
		btni.addActionListener(this);
		btni.setActionCommand("=");
		
		btpm = new JButton("+/-");
		btpm.setBounds(0, 440, 80, 80);
		btpm.setFont(new Font("メイリオ",Font.BOLD,20));
		btpm.addActionListener(this);
		btpm.setActionCommand("plusminus");
		
		btpo = new JButton(".");
		btpo.setBounds(160, 440, 80, 80);
		btpo.setFont(new Font("メイリオ",Font.BOLD,40));
		btpo.addActionListener(this);
		btpo.setActionCommand("point");
		
		btna = new JButton("余り");
		btna.setBounds(160, 120, 80, 80);
		btna.setFont(new Font("メイリオ",Font.BOLD,20));
		btna.addActionListener(this);
		btna.setActionCommand("amari");
		
		btnc = new JButton("C");
		btnc.setBounds(80, 120, 80, 80);
		btnc.setFont(new Font("メイリオ",Font.BOLD,40));
		btnc.addActionListener(this);
		btnc.setActionCommand("clear");
		
		btnr = new JButton("√");
		btnr.setBounds(0, 120, 80, 80);
		btnr.setFont(new Font("メイリオ",Font.BOLD,40));
		btnr.addActionListener(this);
		btnr.setActionCommand("route");
		
		l1 = new JLabel("0");
		l1.setBounds(0, 20, 315, 100);
		l1.setFont(new Font("メイリオ",Font.BOLD,40));
		l1.setHorizontalAlignment(JLabel.RIGHT);
		l1.setBackground(Color.BLUE);
		l1.setOpaque(false);
		
		l2 = new JLabel("");
		l2.setBounds(250, 0, 20, 20);
		l2.setFont(new Font("メイリオ",Font.BOLD,20));
		l2.setBackground(Color.GREEN);
		l2.setOpaque(false);

		
		p = new JPanel();
		p.setLayout(null);
		for(int i = 0; i < 10; i++) {
			p.add(btn[i]);
		}
		p.add(btnp);
		p.add(btnm);
		p.add(btnk);
		p.add(btnw);
		p.add(btni);
		p.add(btpm);
		p.add(btpo);
		p.add(btnr);
		p.add(btna);
		p.add(btnc);
		p.add(l1);
		p.add(l2);
		getContentPane().add(p);
	}
	
	public void actionPerformed(ActionEvent e) {
		String cmd = e.getActionCommand();
		
		if(cmd.equals("1")) {
			indic(1);
		}else if(cmd.equals("2")) {
			indic(2);
		}else if(cmd.equals("3")) {
			indic(3);
		}else if(cmd.equals("4")) {
			indic(4);
		}else if(cmd.equals("5")) {
			indic(5);
		}else if(cmd.equals("6")) {
			indic(6);
		}else if(cmd.equals("7")) {
			indic(7);
		}else if(cmd.equals("8")) {
			indic(8);
		}else if(cmd.equals("9")) {
			indic(9);
		}else if(cmd.equals("0")) {
			indic(0);
		}else if(cmd.equals("+")) {
			l2.setText("+");
			ope("tasu");
		}else if(cmd.equals("-")) {
			l2.setText("-");
			ope("hiku");
		}else if(cmd.equals("x")) {
			l2.setText("x");
			ope("kake");
		}else if(cmd.equals("÷")) {
			l2.setText("÷");
			ope("waru");
		}else if(cmd.equals("=")) {
            ce.numInput(Double.parseDouble(l1.getText()));
			l1.setText(ce.keisan());
			if(l1.getText().length() > 11) {
			    l1.setFont(new Font("メイリオ",Font.BOLD,l1.getWidth() / l1.getText().length() * 3 / 2));
			}
			flag = true;
			count = 0;
			l2.setText("");
		}else if(cmd.equals("timer")) {

            hei = ( de.getHeight() - 38 ) / 7;
			wid = ( de.getWidth()  - 16 ) / 4;

			btn[1].setBounds(0, hei * 3, wid, hei);
			btn[2].setBounds(wid, hei * 3, wid, hei);
			btn[3].setBounds(wid * 2, hei * 3, wid, hei);
			btn[4].setBounds(0, hei * 4, wid, hei);
			btn[5].setBounds(wid, hei * 4, wid, hei);
			btn[6].setBounds(wid * 2, hei * 4, wid, hei);
			btn[7].setBounds(0, hei * 5, wid, hei);
			btn[8].setBounds(wid, hei * 5, wid, hei);
			btn[9].setBounds(wid * 2, hei * 5, wid, hei);
			btn[0].setBounds(wid, hei * 6, wid, hei);
			btpm.setBounds(0, hei * 6, wid, hei);
			btpo.setBounds(wid * 2, hei * 6, wid, hei);
			btnp.setBounds(wid * 3, hei * 2, wid, hei);
			btnm.setBounds(wid * 3, hei * 3, wid, hei);
			btnk.setBounds(wid * 3, hei * 4, wid, hei);
			btnw.setBounds(wid * 3, hei * 5, wid, hei);
			btni.setBounds(wid * 3, hei * 6, wid, hei);
			btnr.setBounds(0, hei * 2, wid, hei);
			btnc.setBounds(wid, hei * 2, wid, hei);
			btna.setBounds(wid * 2, hei * 2, wid, hei);
			l1.setBounds(0, hei / 4, de.getWidth() - 20, hei * 2 - hei / 2);
		}else if(cmd.equals("route")) {
			double rou = 0;
			rou = Double.parseDouble(l1.getText());
			rou = Math.sqrt(rou);
			l1.setText(String.format("%.10f", rou));
		}else if(cmd.equals("clear")) {
			ce.clear();
			l1.setText("0");
			l2.setText("");
    		l1.setFont(new Font("メイリオ",Font.BOLD,40));
		}
	}
	
	public void indic(int but) {
		if(flag) {
			l1.setText("0");
			flag = false;
		}
		if(l1.getText().equals("0")) {
			l1.setText("");
		}
		String str = l1.getText();
		int len = str.length();
		if(len < 11) {
            str = str + but;
			l1.setText(str);
		}
	}
	
	public void ope(String oep) {
		ce.numInput(Double.parseDouble(l1.getText()));
		if(count == 0) {
			ce.opeSet(oep);
			count++;
		}else {
			ce.opeSet(oep);
			l1.setText(ce.keisan());
			if(l1.getText().length() > 11) {
			    l1.setFont(new Font("メイリオ",Font.BOLD,l1.getWidth() / l1.getText().length() * 3 / 2));
			}
		}
		flag = true;
	}
	
	
}
