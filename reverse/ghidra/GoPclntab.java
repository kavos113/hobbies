import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.mem.MemoryAccessException;
import ghidra.program.model.mem.MemoryBlock;

import java.util.Map;

public class GoPclntab extends GhidraScript {

    private enum GoVersion {
        GO_120,
        GO_118,
        GO_116,
        GO_12,
        UNKNOWN
    }

    private static final int GO_120 = 0xFFFFFFF1;
    private static final int GO_118 = 0xFFFFFFF0;
    private static final int GO_116 = 0xFFFFFFFA;
    private static final int GO_12 = 0xFFFFFFFB;

    private static final String PCLNTAB_SECTION_NAME = ".gopclntab";

    private class TabInfo {
        GoVersion version;
        Address base;
        Address textBase;
        int magic;
        int ptrSize;
        int nFunctab;
        int nFiletab;
        Address pFuncnametab;
        Address pCutab;
        Address pFuncData;
        Address pFunctab;
        Address pFiletab;
        Address pPctab;

        Map<Address, String> funcName = new java.util.HashMap<>();

        private final Memory mem = currentProgram.getMemory();

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[+] TabInfo:\n");
            sb.append("    Base: ").append(base).append("\n");
            sb.append("    Magic: ").append(Integer.toHexString(magic)).append("\n");
            sb.append("    Pointer Size: ").append(ptrSize).append("\n");
            sb.append("    nFunctab: ").append(nFunctab).append("\n");
            sb.append("    nFiletab: ").append(nFiletab).append("\n");
            sb.append("    pFuncnametab: ").append(pFuncnametab).append("\n");
            sb.append("    pCutab: ").append(pCutab).append("\n");
            sb.append("    pFuncData: ").append(pFuncData).append("\n");
            sb.append("    pFunctab: ").append(pFunctab).append("\n");
            sb.append("    pFiletab: ").append(pFiletab).append("\n");
            sb.append("    pPctab: ").append(pPctab).append("\n");
            return sb.toString();
        }

        public Address offset(int index) {
            return base.add(8 + (long) ptrSize * index);
        }

        public int funcTableFieldSize() {
            if (version == GoVersion.GO_120 || version == GoVersion.GO_118) {
                return 4;
            }

            return ptrSize;
        }

        public Address getAddressByOffset(int index) throws MemoryAccessException {
            return toAddr(mem.getLong(offset(index)) + base.getOffset());
        }

        public Address funcData(int index) throws MemoryAccessException {
            Address offsetAddr = pFunctab.add((2L * index + 1) * funcTableFieldSize());
            int offset = mem.getInt(offsetAddr);

            return pFuncData.add(offset);
        }

        public int funcDataField(Address func, int fieldIndex) throws MemoryAccessException {
            if (fieldIndex <= 0 || fieldIndex > 9) {
                throw new IllegalArgumentException("fieldIndex must be between 0 and 2");
            }

            int sz0 = ptrSize;
            if (version == GoVersion.GO_120 || version == GoVersion.GO_118) {
                sz0 = 4;
            }

            int offset = sz0 + (fieldIndex - 1) * 4;
            return mem.getInt(func.add(offset));
        }

        private Address functionAddress(int index) throws MemoryAccessException {
            long offset = 2L * index * funcTableFieldSize();
            Address funcAddr = toAddr(mem.getInt(pFunctab.add(offset)));
            if (version == GoVersion.GO_120 || version == GoVersion.GO_118) {
                return funcAddr.add(textBase.getOffset());
            } else {
                return funcAddr;
            }
        }
    }

    @Override
    public void run() throws Exception {
        Address pclntab = findPclntab();
        if (pclntab == null) {
            println("[-] Could not find .gopclntab section. Exiting.");
            return;
        }

        int magic = currentProgram.getMemory().getInt(pclntab);
        println("[+] Magic value: " + Integer.toHexString(magic));

        TabInfo info = null;

        switch (magic) {
            case GO_120:
                println("[+] Go 1.20 detected");
                info = parsePclntab120(pclntab);
                break;
            case GO_118:
                println("[+] Go 1.18 detected");
                break;
            case GO_116:
                println("[+] Go 1.16 detected");
                break;
            case GO_12:
                println("[+] Go 1.2 detected");
                break;
            default:
                println("[-] Unknown Go version");
        }

        if (info != null) {
            println(info.toString());

            for (Map.Entry<Address, String> entry : info.funcName.entrySet()) {
                renameFunction(entry.getKey(), entry.getValue());
            }
        }
    }

    private Address findPclntab() {
        MemoryBlock block = currentProgram.getMemory().getBlock(PCLNTAB_SECTION_NAME);
        if (block != null) {
            println("[+] Found .gopclntab section at " + block.getStart());
            return block.getStart();
        }

        println("[-] Could not find .gopclntab section.");
        return null;
    }

    private TabInfo parsePclntab120(Address pclntab) throws MemoryAccessException {
        Memory mem = currentProgram.getMemory();

        TabInfo info = new TabInfo();

        int magic = mem.getInt(pclntab);
        int ptrSize = mem.getByte(pclntab.add(7));

        info.version = GoVersion.GO_120;
        info.base = pclntab;
        info.magic = magic;
        info.ptrSize = ptrSize;

        int nFunctab = mem.getInt(info.offset(0));
        int nFiletab = mem.getInt(info.offset(1));
        Address textBase = toAddr(mem.getLong(info.offset(2)));
        Address pFuncnametab = info.getAddressByOffset(3);
        Address pCutab = info.getAddressByOffset(4);
        Address pFiletab = info.getAddressByOffset(5);
        Address pPctab = info.getAddressByOffset(6);
        Address pFunctab = info.getAddressByOffset(7);
        Address pFuncData = info.getAddressByOffset(7);
        info.nFunctab = nFunctab;
        info.nFiletab = nFiletab;
        info.textBase = textBase;
        info.pFuncnametab = pFuncnametab;
        info.pCutab = pCutab;
        info.pFiletab = pFiletab;
        info.pPctab = pPctab;
        info.pFunctab = pFunctab;
        info.pFuncData = pFuncData;

        for (int i = 0; i < nFunctab; i++) {
            Address funcDataAddr = info.funcData(i);
            int nameOffset = info.funcDataField(funcDataAddr, 1);
            Address funcNameAddr = info.pFuncnametab.add(nameOffset);
            String funcName = readString(funcNameAddr);

            Address funcAddr = info.functionAddress(i);

            info.funcName.put(funcAddr, funcName);
        }

        return info;
    }

    private String readString(Address addr) throws MemoryAccessException {
        StringBuilder sb = new StringBuilder();
        Memory mem = currentProgram.getMemory();
        byte b;
        while ((b = mem.getByte(addr)) != 0) {
            sb.append((char) b);
            addr = addr.add(1);
        }
        return sb.toString();
    }

    private void renameFunction(Address funcAddr, String funcName) {
        try {
            currentProgram.getFunctionManager().getFunctionAt(funcAddr).setName(funcName, ghidra.program.model.symbol.SourceType.USER_DEFINED);
            println("[+] Renamed function at " + funcAddr + " to " + funcName);
        } catch (Exception e) {
            println("[-] Failed to rename function at " + funcAddr + " to " + funcName);
        }
    }
}