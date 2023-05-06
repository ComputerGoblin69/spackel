use anyhow::Result;

pub trait Stack {
    type Item: Copy;

    fn push(&mut self, element: Self::Item);

    fn pop(&mut self) -> Result<Self::Item>;

    fn dup(&mut self) -> Result<()> {
        let v = self.pop()?;
        self.push(v);
        self.push(v);
        Ok(())
    }

    fn swap(&mut self) -> Result<()> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(b);
        self.push(a);
        Ok(())
    }

    fn over(&mut self) -> Result<()> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a);
        self.push(b);
        self.push(a);
        Ok(())
    }

    fn nip(&mut self) -> Result<()> {
        let b = self.pop()?;
        self.pop()?;
        self.push(b);
        Ok(())
    }

    fn tuck(&mut self) -> Result<()> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(b);
        self.push(a);
        self.push(b);
        Ok(())
    }
}
